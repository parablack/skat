import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.List as List
import Data.Map  as Map
import Data.Text
import Data.Text.Encoding
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
maxElem = List.foldr (max . Just) Nothing

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
    n = List.length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1, n) xs

{- ============ server logic ======================== -}

data ClientData = ClientData
  { dataName     :: String,
    dataRole     :: PlayerPosition,
    dataResigned :: Bool
  }

data ServerData = ServerData
  { dataClients   :: Map.Map Client ClientData,
    dataSkatState :: SkatState
  }

type ServerState = StateT ServerData IO


initialServer :: ServerData
initialServer = ServerData
  { dataClients = Map.empty,
    dataSkatState = ramschFromShuffledDeck deck
  }

newGame :: ServerState ()
newGame = do
  shuffled <- shuffle deck
  modify (\state ->
        state {dataSkatState = ramschFromShuffledDeck shuffled}
    )
  clients <- getClients
  forM_ clients (\client ->
      modifyClient client (\record -> record {dataResigned = False})
    )

replyError :: Client -> String -> ServerState ()
replyError client err =
  reply client . encode . object $ [ pack "error" .= err ]


-- TODO(pinguly+simon): bessere monade für client lookups

modifyClientsMap :: (Map.Map Client ClientData -> Map.Map Client ClientData) -> ServerState ()
modifyClientsMap modifier =
  modify (\state -> state {dataClients = modifier (dataClients state)})

getClients :: ServerState [Client]
getClients = keys . dataClients <$> get

getClientRecords :: ServerState [ClientData]
getClientRecords = elems . dataClients <$> get

lookupClient :: Client -> MaybeT ServerState ClientData
lookupClient client = MaybeT (Map.lookup client . dataClients <$> get)

addClient :: Client -> ClientData -> ServerState ()
addClient client = modifyClientsMap . Map.insert client

removeClient :: Client -> ServerState ()
removeClient = modifyClientsMap . Map.delete

modifyClient :: Client -> (ClientData -> ClientData) -> ServerState ()
modifyClient client modifier =
  modifyClientsMap (\clientsMap ->
      case Map.lookup client clientsMap of
          Just record -> Map.insert client (modifier record) clientsMap
          Nothing     -> clientsMap
    )

-- play :: SkatState -> PlayerPosition -> Card -> Hopefully SkatState

joinPositionName :: ServerState (Map.Map String String)
joinPositionName = Map.fromList . List.map format <$> getClientRecords
  where format entry = (show (dataRole entry), dataName entry)

countResigned :: ServerState Int
countResigned =
  List.length . List.filter id . List.map dataResigned <$> getClientRecords


handlePlayerAction :: Client -> ReceivePacket -> ExceptT String ServerState ()
handlePlayerAction client (PlayCard card) = do
  record <- maybeToExceptT "Not connected!" $ lookupClient client
  skatState <- dataSkatState <$> get
  newState <- ExceptT . return $ play skatState (dataRole record) card
  lift $ modify (\state -> state {dataSkatState = newState})

handlePlayerAction client (SetName name) =
  lift $ modifyClient client (\record -> record {dataName = name})

handlePlayerAction client Resign = lift $ do
    modifyClient client (\record -> record {dataResigned = True})
    numResigned <- countResigned
    numPlayer <- List.length <$> getClients
    if numResigned >= numPlayer
        then newGame
        else return ()

handlePlayerAction _ _ = throwError "Not implemented yet!"

-- TODO: PlayVariant GameMode | ShowCards | DiscardSkat Card Card

broadcastState :: ServerState ()
broadcastState = do
  clients <- Map.assocs . dataClients <$> get
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
  assignedPositions <- List.map dataRole <$> getClientRecords
  let clientPos = List.head $ [Geber, Vorhand, Mittelhand] List.\\ assignedPositions
  addClient client ClientData {
      dataName     = "Anon",
      dataRole     = clientPos,
      dataResigned = False
    }
  broadcastState

handleEvent (Disconnect client cause) = do
  println $ (show client) ++ " " ++ show cause ++ ": disconnected!"
  removeClient client

handleEvent (Message client message) = do
  let maybeTAction = MaybeT . return . decodeStrict . encodeUtf8 . pack $ message
  result <- runExceptT $ do
      action <- maybeToExceptT "couldn't deserialize action" maybeTAction
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
