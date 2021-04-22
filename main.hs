import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Map
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Definitions
import Network.WebSockets
import System.Random
import Data.Array.IO
import Serializer
import Skat

type ClientId = Int

data Client = Client
  { clientId :: ClientId,
    clientConn :: Connection
  }


data ServerData = ServerData
  { clientNames :: Data.Map.Map ClientId String,
    clientRoles :: Data.Map.Map ClientId PlayerPosition,
    skatState :: SkatState,
    clients :: Data.Map.Map ClientId Client
  }

type ServerState = StateT ServerData IO

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
    newArray n xs =  newListArray (1,n) xs


initialServer = do
  shuffled <- shuffle deck
  return ServerData
    { skatState = ramschFromShuffledDeck shuffled,
      clients = Data.Map.empty,
      clientRoles = Data.Map.empty,
      clientNames = Data.Map.empty
    } :: IO ServerData

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

println :: String -> ServerState ()
println = lift . putStrLn

reply :: WebSocketsData a => Client -> a -> ServerState ()
reply client = lift . sendTextData (clientConn client)

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

disconnect :: Client -> ServerState ()
disconnect client = lift $ sendClose (clientConn client) Data.Text.empty

data Event
  = Connect Client
  | Disconnect Client ConnectionException
  | Message Client String

recvEvent :: Client -> IO Event
recvEvent client = do
  text <- receiveData $ clientConn client
  return $ Message client (Data.Text.unpack text)

generateEvents :: Client -> (Event -> IO ()) -> IO ()
generateEvents client notify =
  do
    notify $ Connect client
    forever (recvEvent client >>= notify)
    `catch` (notify . Disconnect client)

takeId :: MVar [ClientId] -> IO (Maybe ClientId)
takeId ids = modifyMVar ids modify
  where
    modify (cid : others) = return (others, Just cid)
    modify [] = return ([], Nothing)

putId :: MVar [ClientId] -> ClientId -> IO ()
putId ids cid = modifyMVar_ ids $ return . (cid :)

acceptClient :: MVar [ClientId] -> Chan Event -> PendingConnection -> IO ()
acceptClient availIds eventQ pending = do
  cid <- takeId availIds
  case cid of
    Just num -> do
      connection <- acceptRequest pending
      generateEvents
        (Client {clientId = num, clientConn = connection})
        (writeChan eventQ)
      putId availIds num
    Nothing -> do
      rejectRequestWith pending defaultRejectRequest

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

handlePlayerAction :: ClientId -> ReceivePacket -> ServerData -> Hopefully ServerData
handlePlayerAction clientId (PlayCard card) state = do
  let player = clientRoles state ! clientId
  newState <- play (skatState state) player card
  return state {skatState = newState}
handlePlayerAction clientId (SetName name) state = do
  let newNames = Data.Map.insert clientId name (clientNames state)
  return state {clientNames = newNames}
handlePlayerAction _ _ _ = Left "reizen not implemented"

-- TODO: PlayVariant GameMode | ShowCards | DiscardSkat Card Card

handleEvent :: Event -> ServerState ()
handleEvent event = do
  case event of
    Connect client -> do
      println $ "Client " ++ show (clientId client) ++ ": connected!"
      updateClients (Data.Map.insert (clientId client) client)
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
            Right newState -> put newState
            Left error -> replyError client error
  -- send out updated state
  clientList <- elems . clients <$> get
  skatState <- skatState <$> get
  _clientRoles <- clientRoles <$> get
  _clientNames <- clientNames <$> get
  forM_
    clientList
    ( \client ->
        let player = (_clientRoles ! clientId client)
            namemap = joinPositionName _clientRoles _clientNames
         in reply client (encode (SkatStateForPlayer player skatState namemap))
    )

-- TODO(pinguly+simon): handle client disconnect(gemacht??) & bessere monade für client lookups
-- TODO(pinguly) token zurückgeben bei disconnect
main = do
  eventQ <- newChan :: IO (Chan Event)
  availIds <- newMVar [1 .. 3] :: IO (MVar [ClientId])

  putStrLn "Listening on 0.0.0.0:8080"

  forkIO $ runServer "0.0.0.0" 8080 $ acceptClient availIds eventQ

  initialServer <- initialServer
  evalStateT
    (forever $ (lift . readChan) eventQ >>= handleEvent)
    initialServer
