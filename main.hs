import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Map
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Definitions
import System.Random
import Data.Array.IO
import Serializer
import Server
import Skat

{- ============ util ======================== -}

println :: MonadIO m => String -> m ()
println = liftIO . putStrLn

maxElem :: Ord a => [a] -> Maybe a
maxElem = Data.List.foldr (max . Just) Nothing

shuffle :: MonadIO m => [a] -> m [a]
shuffle xs = liftIO $ do
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

{- ============ server logic ======================== -}

data ClientData = ClientData
  { dataName     :: String,
    dataRole     :: PlayerPosition,
    dataResigned :: Bool
  }

data ServerData = ServerData
  { dataClients   :: Map Client ClientData,
    dataSkatState :: SkatState
  }

type ServerState = StateT ServerData IO

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

initialServer :: ServerData
initialServer = ServerData
  { dataClients = Data.Map.empty,
    dataSkatState = ramschFromShuffledDeck deck
  }

newGame :: ServerState ()
newGame = do
  shuffled <- shuffle deck
  modify (\state ->
        state {dataSkatState = ramschFromShuffledDeck shuffled}
    )
  clients <- keys . dataClients <$> get
  forM_ clients (\client ->
      modifyClient client (\record -> record {dataResigned = False})
    )

replyError :: Client -> String -> ServerState ()
replyError client err =
  reply client . encode . object $ [ Data.Text.pack "error" .= err ]


-- TODO(pinguly+simon): bessere monade für client lookups

modifyClients :: (Map Client ClientData -> Map Client ClientData) -> ServerState ()
modifyClients modifier =
  modify (\state -> state {dataClients = modifier (dataClients state)})

lookupClient :: Client -> MaybeT ServerState ClientData
lookupClient client = MaybeT (Data.Map.lookup client . dataClients <$> get)

addClient :: Client -> ClientData -> ServerState ()
addClient client = modifyClients . Data.Map.insert client

removeClient :: Client -> ServerState ()
removeClient = modifyClients . Data.Map.delete

modifyClient :: Client -> (ClientData -> ClientData) -> ServerState ()
modifyClient client modifier =
  modifyClients (\clients ->
      case Data.Map.lookup client clients of
          Just record -> Data.Map.insert client (modifier record) clients
          Nothing     -> clients
    )

-- play :: SkatState -> PlayerPosition -> Card -> Hopefully SkatState

joinPositionName :: ServerState (Data.Map.Map String String)
joinPositionName =
  Data.Map.fromList . Data.List.map format . elems . dataClients <$> get
  where
    format entry = (show (dataRole entry), dataName entry)

countResigned :: ServerState Int
countResigned =
  Data.List.length . Data.List.filter id . Data.List.map dataResigned . elems . dataClients <$> get


handlePlayerAction :: Client -> ReceivePacket -> ExceptT String ServerState ()
handlePlayerAction client (PlayCard card) = do
  record <- maybeToExceptT "Not connected!" $ lookupClient client
  skatState <- dataSkatState <$> get
  newState <- liftEither $ play skatState (dataRole record) card
  lift $ modify (\state -> state {dataSkatState = newState})

handlePlayerAction client (SetName name) =
  lift $ modifyClient client (\record -> record {dataName = name})

handlePlayerAction client Resign = lift $ do
    modifyClient client (\record -> record {dataResigned = True})
    numResigned <- countResigned
    numPlayer <- Data.List.length . elems . dataClients <$> get
    if numResigned >= numPlayer
        then newGame
        else return ()

handlePlayerAction _ _ = throwError "Not implemented yet!"

-- TODO: PlayVariant GameMode | ShowCards | DiscardSkat Card Card

broadcastState :: ServerState ()
broadcastState = do
  clients <- Data.Map.assocs . dataClients <$> get
  skatState <- dataSkatState <$> get
  numResigned <- countResigned
  forM_ clients ( \(client, record) -> do
      let player = dataRole record
      namemap <- joinPositionName
      reply client (encode (SkatStateForPlayer player skatState namemap numResigned))
    )

handleEvent :: Event -> ServerState ()
handleEvent (Connect client) = do
  println $ (show client) ++ ": connected!"
  assignedPositions <- Data.List.map dataRole . elems . dataClients <$> get
  let clientPos = Data.List.head $ [Geber, Vorhand, Mittelhand] Data.List.\\ assignedPositions
  addClient client ClientData {
      dataName     = "Anon",
      dataRole     = clientPos,
      dataResigned = False
    }
  broadcastState

handleEvent (Disconnect client cause) = do
  println $ (show client) ++ " " ++ show cause ++ ": disconnected!"
  removeClient client

handleEvent (Message client s) = do
  -- TODO(bennofs): fix
  let message = B.fromStrict (Data.Text.Encoding.encodeUtf8 (Data.Text.pack s))
  let maybeAction = MaybeT . return . decode $ message
  result <- runExceptT $ do
      action <- maybeToExceptT "couldn't deserialize action" maybeAction
      handlePlayerAction client action
  case result of
      Left err -> replyError client err
      Right _  -> return ()
  broadcastState

main = do
  putStrLn "Listening on 0.0.0.0:8080"
  evalStateT
      (newGame >> runWebSockServer defaultServerConfig handleEvent)
      initialServer
