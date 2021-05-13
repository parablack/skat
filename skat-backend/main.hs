{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import System.IO

import Serializer()
import GameServer.Definitions
import GameServer.Server
import Util
import WebSockServer


replyError :: MonadIO m => Client -> String -> m ()
replyError client err =
  reply client . encode . object $ [ pack "error" .= err ]


handleEvent :: Event -> ExceptT String (StateT ServerData IO) ()
handleEvent (Connect client) = do
    let user = User (show client)
    registerUser user "Anon" (reply client . encode)
    println $ (show user) ++ " added!"

handleEvent (Disconnect client cause) = do
  let user = User (show client)
  unregisterUser user
  println $ (show user) ++ " removed! (" ++ show cause ++ ")"

handleEvent (Message client message) = do
    let user = User (show client)
    let maybeTAction = MaybeT . return . decodeStrict . encodeUtf8 . pack $ message
    result <- runExceptT $ do
        action <- maybeToExceptT "Couldn't deserialize action!" maybeTAction
        handleUserAction user action
    case result of
      Left err -> replyError client err
      Right _  -> return ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Listening on 0.0.0.0:8080"
  err <- flip evalStateT emptyServerData $ runExceptT $ do
            registerLobby (Lobby 1) "Lobby 1"
            registerLobby (Lobby 2) "Lobby 2"
            runWebSockServer defaultServerConfig handleEvent
  case err of
      Left message -> println $ "Server terminated because of error: " ++ message
      Right ()     -> println $ "Server terminated witout error."
