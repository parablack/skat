{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase #-}

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.List as List
import Data.Text
import Data.Text.Encoding
import System.IO

import Serializer()
import Skat.Definitions
import GameServer.Definitions
import GameServer.Protocol
import GameServer.Server
import Util
import WebSockServer


type AI = Chan (Either String GameResponse) -> (GameRequest-> IO ()) -> IO ()


-- TODO copied
isCardAllowed :: GameMode -> Stich -> [Card] -> Card -> Bool
isCardAllowed gm stich myCards card =
    case stich of
        [] -> True
        lst -> let firstCardInStich = fst $ List.head lst
                   compatCheck      = cardsCompatible gm
                in
                    -- alles easy, Karte passt
                    compatCheck firstCardInStich card
                    -- drauflegen falls keine Karte mehr dort ist
                    || not (List.any (compatCheck firstCardInStich) myCards)

makeSimpleAIMove :: GameResponse -> IO (Maybe GameRequest)
makeSimpleAIMove (StatePlayerResponse phaseInfo publicInfo privateInfo) =
    if infoNumResigned publicInfo > 0 && not (infoResigned privateInfo) then
        return . Just $ Resign
    else if not (infoYourTurn privateInfo) then
        return Nothing
    else case phaseInfo of
        phase@ReizPhaseInfo{} ->
            if infoIsAnsagerTurn phase
                then return . Just . MakeMove $ ReizBid Weg
                else return . Just . MakeMove $ ReizAnswer False
        PickingPhaseInfo{} ->
            return Nothing

        phase@RunningPhaseInfo{} ->
            let
                myCards   = infoYourCards privateInfo
                mode      = infoGameMode phase
                currStich = infoCurrentStich phase
                isAllowed  = isCardAllowed mode currStich myCards
            in do
                possibleCards <- shuffle . List.filter isAllowed $ myCards
                return . Just . MakeMove . PlayCard . List.head $ possibleCards

        FinishedPhaseInfo{} ->
            return Nothing

makeSimpleAIMove _ = return Nothing

simpleAI :: AI
simpleAI inChan sendRequest =
    forever $ readChan inChan >>=
        \case
            Left err       -> println $ "Simple AI error: " ++ err
            Right response ->
                makeSimpleAIMove response >>= \case
                Just request -> do
                    threadDelay 3000000 -- TODO
                    sendRequest request
                Nothing      -> return ()

registerBot
    :: (MonadIO m, MonadError String m, MonadState ServerData m)
    => User -> String -> Chan GameServerMessage -> AI -> m ()
registerBot user name outChan ai = do
    inChan <- liftIO $ (newChan :: IO (Chan (Either String GameResponse)))
    _ <- liftIO . forkIO $ ai inChan $ \req ->
        writeChan outChan $ Request user req (writeChan inChan . Left)
    registerUser user name (writeChan inChan. Right)

data GameServerMessage
    = Register User String (GameResponse -> IO ())
    | Unregister User String
    | Request User GameRequest (String -> IO ())


replyError :: MonadIO m => Client -> String -> m ()
replyError client err =
  reply client . encode . object $ [ pack "error" .= err ]

handleEvent ::  Chan GameServerMessage -> Event -> IO ()
handleEvent chan (Connect client) = do
    let user = User (show client)
    writeChan chan $ Register user "Anon" (reply client . encode)

handleEvent chan (Disconnect client cause) = do
  let user = User (show client)
  writeChan chan $ Unregister user (show cause)

handleEvent chan (Message client message) = do
    let user = User (show client)
    let maybeAction = decodeStrict . encodeUtf8 . pack $ message
    case maybeAction of
      Nothing -> replyError client "Couldn't deserialize action!"
      Just action  ->
        writeChan chan $ Request user action (replyError client)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    gameServerChan <- newChan :: IO (Chan GameServerMessage)

    _ <- forkIO $ runWebSockServer defaultServerConfig (handleEvent gameServerChan)


    err <- flip evalStateT emptyServerData $ runExceptT $ do
        registerLobby (Lobby 1) "Lobby 1"
        registerLobby (Lobby 2) "Lobby 2"

        let bot1 = User "Bot 1"
        registerBot bot1 "Simple Bot \129302" gameServerChan simpleAI
        handleUserAction bot1 $ JoinLobby 2 Geber

        forever $ (liftIO . readChan $ gameServerChan) >>=
            \case
                Register user name onReply -> do
                    registerUser user name onReply
                    println $ (show user) ++ " added!"

                Unregister user cause -> do
                    unregisterUser user
                    println $ (show user) ++ " removed! (" ++ show cause ++ ")"

                Request user request onError -> do
                    res <- runExceptT (handleUserAction user request)
                    case res of
                        Left err -> liftIO (onError err)
                        Right _  -> return ()

    case err of
        Left message -> println $ "Server terminated because of error: " ++ message
        Right ()     -> println $ "Server terminated witout error."
