import Control.Monad
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Map
import qualified Data.Set as Set
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Definitions
import System.Random
import Data.Array.IO
import Serializer
import Server
import Skat


data ServerData = ServerData
  { clientNames   :: Data.Map.Map ClientId String,
    clientRoles   :: Data.Map.Map ClientId PlayerPosition,
    clientsResign :: Set.Set ClientId,
    skatState     :: SkatState,
    clients       :: Data.Map.Map ClientId Client
  }

type ServerState = StateT ServerData IO

println :: String -> ServerState ()
println = lift . putStrLn

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = Data.List.length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1, n) xs


initialServer = do
  shuffled <- shuffle deck
  return ServerData
    { skatState = ramschFromShuffledDeck shuffled,
      clients = Data.Map.empty,
      clientRoles = Data.Map.empty,
      clientNames = Data.Map.empty,
      clientsResign = Set.empty
    } :: IO ServerData

newGame :: ServerData -> IO ServerData
newGame sdata = do
  shuffled <- shuffle deck
  return sdata {
    skatState = ramschFromShuffledDeck shuffled
  }

{-
initialServer = do
  return ServerData
    { skatState = GameFinishedState {
      players = playersFromDeck deck,
      lastStich = [],
      scores = Data.Map.insert Vorhand 1000 $ Data.Map.singleton Geber 100,
      winner = Geber
    },
      clients = Data.Map.empty,
      clientRoles = Data.Map.empty,
      clientNames = Data.Map.empty
    } :: IO ServerData
-}

replyError :: Client -> String -> ServerState ()
replyError client error =
  reply
    client
    ( encode
        ( object
            [ Data.Text.pack "error" .= error
            ]
        )
    )

-- TODO(pinguly+simon): bessere monade für client lookups

updateClients :: (Data.Map.Map ClientId Client -> Data.Map.Map ClientId Client) -> ServerState ()
updateClients modifier = modify (\state -> state {clients = modifier (clients state)})

updateClientRoles :: (Data.Map.Map ClientId PlayerPosition -> Data.Map.Map ClientId PlayerPosition) -> ServerState ()
updateClientRoles modifier = modify (\state -> state {clientRoles = modifier (clientRoles state)})

updateClientNames :: (Data.Map.Map ClientId String -> Data.Map.Map ClientId String) -> ServerState ()
updateClientNames modifier = modify (\state -> state {clientNames = modifier (clientNames state)})

maxElem :: Ord a => [a] -> Maybe a
maxElem = Data.List.foldr (max . Just) Nothing

-- play :: SkatState -> PlayerPosition -> Card -> Hopefully SkatState

joinPositionName :: Data.Map.Map ClientId PlayerPosition -> Data.Map.Map ClientId String -> Data.Map.Map String String
joinPositionName pos = mapKeys (show . (pos !))

handlePlayerAction :: ClientId -> ReceivePacket -> ServerData -> Hopefully (IO ServerData)
handlePlayerAction clientId (PlayCard card) state = do
  let player = clientRoles state ! clientId
  newState <- play (skatState state) player card
  return $ return (state {skatState = newState})
handlePlayerAction clientId (SetName name) state = do
  let newNames = Data.Map.insert clientId name (clientNames state)
  return $ return (state {clientNames = newNames})
handlePlayerAction clientId Resign state =
  let newstate = state {
                    clientsResign = Set.insert clientId (clientsResign state)
                 }
  in  if Set.size (clientsResign newstate) >= Data.Map.size (clientRoles state)  then

        return $ newGame (state {clientsResign = Set.empty})
      else return $ return newstate
handlePlayerAction _ _ _ = Left "not implemented yet"

-- TODO: PlayVariant GameMode | ShowCards | DiscardSkat Card Card

handleEvent :: Event -> ServerState ()
handleEvent event = do
  case event of
    Connect client -> do
      println $ "Client " ++ show (clientId client) ++ ": connected!"
      updateClients (Data.Map.insert (clientId client) client)
      updateClientNames (Data.Map.insert (clientId client) "Anon")
      assignedPositions <- elems . clientRoles <$> get
      let clientPos = Data.List.head $ [Geber, Vorhand, Mittelhand] Data.List.\\ assignedPositions
      updateClientRoles (Data.Map.insert (clientId client) clientPos)
    Disconnect client cause -> do
      println $ "Client " ++ show (clientId client) ++ " " ++ show cause ++ ": disconnected!"
      updateClients (Data.Map.delete (clientId client))
      updateClientRoles (Data.Map.delete (clientId client))
      updateClientNames (Data.Map.delete (clientId client))
    Message client s -> do
      -- TODO(bennofs): fix
      let action = decode (B.fromStrict (Data.Text.Encoding.encodeUtf8 (Data.Text.pack s))) :: Maybe ReceivePacket
      case action of
        Nothing -> replyError client "couldn't deserialize action"
        Just action -> do
          state <- get
          case handlePlayerAction (clientId client) action state of
            Right newState -> put =<< lift newState
            Left error -> replyError client error
  -- send out updated state
  clientList <- elems . clients <$> get
  skatState <- skatState <$> get
  _clientRoles <- clientRoles <$> get
  _clientNames <- clientNames <$> get
  _clientsResign <- Set.size <$> (clientsResign <$> get)
  forM_
    clientList
    ( \client ->
        let player = (_clientRoles ! clientId client)
            namemap = joinPositionName _clientRoles _clientNames
         in reply client (encode (SkatStateForPlayer player skatState namemap _clientsResign))
    )

main = do
  putStrLn "Listening on 0.0.0.0:8080"
  initialServer <- initialServer
  evalStateT
      (runWebSockServer defaultServerConfig handleEvent)
      initialServer
