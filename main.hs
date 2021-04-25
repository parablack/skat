{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

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
type MonadServerState = MonadState ServerData

initialServer :: ServerData
initialServer = ServerData
  { dataClients = Map.empty,
    dataSkatState = initialStateFromDeck deck
  }

newGame :: (MonadServerState m, MonadIO m) => m ()
newGame = do
  shuffled <- shuffle deck
  modify (\state ->
        state {dataSkatState = initialStateFromDeck shuffled}
    )
  clients <- getClients
  forM_ clients (\client ->
      modifyClient client (\record -> record {dataResigned = False})
    )

replyError :: MonadIO m => Client -> String -> m ()
replyError client err =
  reply client . encode . object $ [ pack "error" .= err ]


-- TODO(pinguly+simon): bessere monade für client lookups

modifyClientsMap :: MonadServerState m => (Map.Map Client ClientData -> Map.Map Client ClientData) -> m ()
modifyClientsMap modifier =
  modify (\state -> state {dataClients = modifier (dataClients state)})

getClients :: MonadServerState m => m [Client]
getClients = keys . dataClients <$> get

getClientRecords :: MonadServerState m => m [ClientData]
getClientRecords = elems . dataClients <$> get

lookupClient :: MonadServerState m => Client -> MaybeT m ClientData
lookupClient client = MaybeT (Map.lookup client . dataClients <$> get)

addClient :: MonadServerState m => Client -> ClientData -> m ()
addClient client = modifyClientsMap . Map.insert client

removeClient :: MonadServerState m => Client -> m ()
removeClient = modifyClientsMap . Map.delete

modifyClient :: MonadServerState m => Client -> (ClientData -> ClientData) -> m ()
modifyClient client modifier =
  modifyClientsMap (\clientsMap ->
      case Map.lookup client clientsMap of
          Just record -> Map.insert client (modifier record) clientsMap
          Nothing     -> clientsMap
    )

-- play :: SkatState -> PlayerPosition -> Card -> Hopefully SkatState

joinPositionName :: MonadServerState m => m (Map.Map String String)
joinPositionName = Map.fromList . List.map format <$> getClientRecords
  where format entry = (show (dataRole entry), dataName entry)

countResigned :: MonadServerState m => m Int
countResigned =
  List.length . List.filter id . List.map dataResigned <$> getClientRecords


handlePlayerAction :: (MonadIO m, MonadServerState m) => Client -> ReceivePacket -> ExceptT String m ()
handlePlayerAction client (PlayCard card) = do
  record <- maybeToExceptT "Not connected!" $ lookupClient client
  skatState <- dataSkatState <$> get
  newState <- liftEither $ play skatState (dataRole record) card
  modify (\state -> state {dataSkatState = newState})

handlePlayerAction client (SetName name) =
  modifyClient client (\record -> record {dataName = name})

handlePlayerAction client Resign = do
  modifyClient client (\record -> record {dataResigned = True})
  numResigned <- countResigned
  numPlayer <- List.length <$> getClients
  when (numResigned >= numPlayer) newGame

handlePlayerAction client (ReizBid val) = do
  record <- maybeToExceptT "Not connected!" $ lookupClient client
  skatState <- dataSkatState <$> get
  newState <- liftEither $ reizen skatState (dataRole record) val
  modify (\state -> state {dataSkatState = newState})

handlePlayerAction client (ReizAnswer val) = do
  record <- maybeToExceptT "Not connected!" $ lookupClient client
  skatState <- dataSkatState <$> get
  newState <- liftEither $ reizenAntwort skatState (dataRole record) val
  modify (\state -> state {dataSkatState = newState})


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
