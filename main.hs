{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Aeson
import Data.List as List
import Data.Map  as Map
import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.Text
import Data.Text.Encoding
import System.IO

import Definitions
import Serializer
import Skat
import SkatServer
import Util
import WebSockServer


data MainData = MainData
  { dataServerData :: ServerData,
    dataClientMap  :: Map.Map Client SkatServer.Player,
    dataAvailPositions :: [PlayerPosition]
  }

runServer :: ExceptT String (StateT ServerData IO) a ->  (StateT MainData IO) (Either String a)
runServer monad = do
    serverData <- dataServerData <$> get
    (err, newData) <- lift $ runStateT (runExceptT monad) serverData
    modify (\record -> record{ dataServerData = newData })
    return err

replyError :: MonadIO m => Client -> String -> m ()
replyError client err =
  reply client . encode . object $ [ pack "error" .= err ]


handleEvent :: Event -> StateT MainData IO ()
handleEvent (Connect client) = do
  println $ (show client) ++ ": connected!"
  result <- runServer $ registerPlayer "Anon" (reply client . encode)
  case result of
      Left message  ->
        replyError client message
      Right player -> do
        nextPos : otherPos <- dataAvailPositions <$> get
        runServer $ handlePlayerAction player (JoinLobby 1 nextPos)
        modify (\record ->
            record {
                dataClientMap = Map.insert client player (dataClientMap record),
                dataAvailPositions = otherPos
                }
            )
        println $ (show player) ++ " added!"
        return ()

handleEvent (Disconnect client cause) = do
  println $ (show client) ++ " " ++ show cause ++ ": disconnected!"
  player <- fromJust . Map.lookup client . dataClientMap <$> get
  println $ (show player) ++ " removed!"
  ePosition <- runServer $ using (Lobby 1) $ lookupPlayerPosition player
  case ePosition of
      Left msg       -> println $ "Error: " ++ msg
      Right position -> modify (\record -> record { dataAvailPositions = position : (dataAvailPositions record) } )
  runServer $ unregisterPlayer player
  modify (\record -> record { dataClientMap = Map.delete client (dataClientMap record) } )

handleEvent (Message client message) = do
    player <- fromJust . Map.lookup client . dataClientMap <$> get
    let maybeTAction = MaybeT . return . decodeStrict . encodeUtf8 . pack $ message
    result <- runExceptT $ do
        action <- maybeToExceptT "couldn't deserialize action" maybeTAction
        ExceptT $ runServer $ handlePlayerAction player action
    case result of
      Left err -> replyError client err
      Right _  -> return ()

main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Listening on 0.0.0.0:8080"
  let initialMainData = MainData emptyServerData Map.empty [Geber, Vorhand, Mittelhand]
  evalStateT (do
      runServer $ createLobby
      runWebSockServer defaultServerConfig handleEvent
      ) initialMainData
