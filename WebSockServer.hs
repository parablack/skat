module WebSockServer (
    Client,
    Event(..),
    ServerConfig(..),
    defaultServerConfig,
    reply,
    disconnect,
    runWebSockServer
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Network.WebSockets

type ClientId = Int

data Client = Client
  { clientId   :: ClientId,
    clientConn :: Connection
  }

instance Show Client where
  show a = "Client " ++ show (clientId a)

instance Eq Client where
  (==) a b = (clientId a) == (clientId b)

instance Ord Client where
  compare a b = compare (clientId a) (clientId b)

data Event
   = Connect Client
   | Disconnect Client ConnectionException
   | Message Client String

data ServerConfig = ServerConfig
  { configAddress :: String,
    configPort    :: Int,
    configNumIds  :: Int
  }

reply :: (MonadIO m, WebSocketsData a) => Client -> a -> m ()
reply client message = do
  _ <- liftIO . try $ sendTextData (clientConn client) message
    :: MonadIO m => m (Either ConnectionException ())
  return ()

disconnect :: MonadIO m => Client -> m ()
disconnect client = do
  _ <- liftIO . try $ sendClose (clientConn client) Data.Text.empty
    :: MonadIO m => m (Either ConnectionException ())
  return ()

recvEvent :: Client -> IO Event
recvEvent client = do
  text <- receiveData $ clientConn client
  return $ Message client (Data.Text.unpack text)

generateEvents :: Client -> (Event -> IO ()) -> IO ()
generateEvents client notify = do
  notify $ Connect client
  forever (recvEvent client >>= notify) `catch` (notify . Disconnect client)

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

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { configAddress = "0.0.0.0",
    configPort    = 8080,
    configNumIds  = 6
  }

runWebSockServer :: MonadIO m => ServerConfig -> (Event -> m ()) -> m ()
runWebSockServer config handleEvent = do
  eventQ   <- liftIO newChan
  let n = configNumIds config
  availIds <- liftIO $ newMVar [1 .. n]

  let run = runServer (configAddress config) (configPort config)
  _ <- liftIO . forkIO . run $ acceptClient availIds eventQ

  forever $ (liftIO . readChan) eventQ >>= handleEvent
