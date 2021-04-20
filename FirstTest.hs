import Control.Monad
import Control.Monad.State.Lazy
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import Data.Text
import Network.WebSockets

type ClientId = Int

data Client = Client {
    clientId   :: ClientId,
    clientConn :: Connection
}

data ServerData = ServerData ()

type ServerState = StateT ServerData IO

initialServer = ServerData ()

println :: String -> ServerState ()
println = lift . putStrLn

reply :: Client -> String -> ServerState ()
reply client = lift . sendTextData (clientConn client) . Data.Text.pack

disconnect :: Client -> ServerState ()
disconnect client = lift $ sendClose (clientConn client) Data.Text.empty


data Event = Connect Client
           | Disconnect Client
           | Message Client String
           | Exception Client ConnectionException

recvEvent :: Client -> IO Event
recvEvent client = do
    text <- receiveData $ clientConn client
    return $ Message client (Data.Text.unpack text)

generateEvents :: Client -> (Event -> IO ()) -> IO ()
generateEvents client notify = do
    notify $ Connect client
    forever (recvEvent client >>= notify) `catch` (notify . Exception client)
    notify $ Disconnect client

takeId :: MVar [ClientId] -> IO (Maybe ClientId)
takeId ids =  modifyMVar ids modify
    where
        modify (cid : others) = return (others, Just cid)
        modify []             = return ([],     Nothing)

putId :: MVar [ClientId] -> ClientId -> IO ()
putId ids cid = modifyMVar_ ids $ return . (cid:)

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

handleEvent :: Event -> ServerState ()
handleEvent event =
    case event of
        Connect client -> do
            println $ "Client " ++ (show $ clientId client) ++ ": connected!"
        Disconnect client -> do
            println $ "Client " ++ (show $ clientId client) ++ ": disconnected!"
        Exception client err -> do
            println $ "Client " ++ (show $ clientId client) ++ ": " ++ (show err)
        Message client text -> do
            println $ "Client " ++ (show $ clientId client) ++ ": " ++ (show text)
            -- reply client $ "You wrote " ++ (show text)
            -- disconnect client

main = do
    eventQ   <- newChan        :: IO (Chan Event)
    availIds <- newMVar [1..3] :: IO (MVar [ClientId])

    forkIO $ runServer "127.0.0.1" 8080 $ acceptClient availIds eventQ

    evalStateT
        (forever $ (lift . readChan) eventQ >>= handleEvent)
        initialServer
