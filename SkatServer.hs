{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module SkatServer (
    Player(..),
    Lobby(..),
    ServerData,
    emptyServerData,
    createLobby,
    registerPlayer,
    unregisterPlayer,
    handlePlayerAction,
    lookupPlayerPosition,
    using
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map  as Map
import Prelude hiding (lookup)

import Definitions hiding (Player)
import Skat
import Util

newtype Player = Player Int
    deriving (Show, Eq, Ord)

newtype Lobby = Lobby Int
    deriving (Show, Eq, Ord)

data PlayerData = PlayerData
  { dataName  :: String,
    dataLobby :: Maybe Lobby,
    dataReply :: SkatStateForPlayer -> IO ()
  }

data PositionData = PositionData
  { dataPlayer    :: Maybe Player,
    dataResigned  :: Bool
  }

data LobbyData = LobbyData
  { dataPositions :: Map.Map PlayerPosition PositionData,
    dataSkatState :: SkatState
  }

data ServerData = ServerData
  { dataPlayers :: Map.Map Player PlayerData,
    dataLobbies :: Map.Map Lobby LobbyData
  }

emptyPositionData :: PositionData
emptyPositionData = PositionData Nothing False

emptyLobbyData :: LobbyData
emptyLobbyData = LobbyData
  { dataPositions = Map.fromList [
        (Geber,      emptyPositionData),
        (Vorhand,    emptyPositionData),
        (Mittelhand, emptyPositionData)
        ],
    dataSkatState = initialStateFromDeck deck
  }

lobbyPositions = [Geber, Vorhand, Mittelhand]

emptyServerData :: ServerData
emptyServerData = ServerData Map.empty (Map.fromList [(Lobby 1, emptyLobbyData)])

class (Show i, Ord i) => ServerRecord i s r | i -> s, i -> r where
    extractMap :: s -> Map.Map i r
    modifyMap :: MonadState s m => (Map.Map i r -> Map.Map i r) -> m ()

    lookup :: (MonadState s m, MonadError String m) => i -> m r
    lookup key = do
        result <- Map.lookup key . extractMap <$> get
        case result of
            Just record -> return record
            Nothing     -> throwError $ (show key) ++ " not found!"

    insert :: MonadState s m => i -> r -> m ()
    insert key record = modifyMap $ Map.insert key record

    delete :: MonadState s m => i -> m ()
    delete key = modifyMap $ Map.delete key

    adjust :: MonadState s m => i -> (r -> r) -> m ()
    adjust key modifier = modifyMap $ Map.adjust modifier key

    using :: (MonadState s m, MonadError String m)
          => i -> ExceptT String (StateT r m) a -> m a
    using key monad = do
        record <- lookup key
        (err, newRecord) <- runStateT (runExceptT monad) record
        case err of
            Left message -> throwError message
            Right result -> insert key newRecord >> return result


instance ServerRecord Player ServerData PlayerData where
    extractMap = dataPlayers
    modifyMap modifier = modify (\server ->
        server{ dataPlayers = modifier (dataPlayers server) })

instance ServerRecord Lobby ServerData LobbyData where
    extractMap = dataLobbies
    modifyMap modifier = modify (\server ->
        server{ dataLobbies = modifier (dataLobbies server) })

instance ServerRecord PlayerPosition LobbyData PositionData where
    extractMap = dataPositions
    modifyMap modifier = modify (\lobby ->
        lobby{ dataPositions = modifier (dataPositions lobby) })


maybeToError :: MonadError String m => String -> Maybe a -> m a
maybeToError message = maybe (throwError message) return

lookupLobby
    :: (MonadState ServerData m, MonadError String m) => Player -> m Lobby
lookupLobby player =
    dataLobby <$> lookup player
    >>= maybeToError (show player ++ " is not in a lobby!")

lookupPlayerPosition
    :: (MonadState LobbyData m, MonadError String m) => Player -> m PlayerPosition
lookupPlayerPosition player = do
    maybeMatch <- List.find hasPlayer . Map.assocs . dataPositions <$> get
    fst <$> maybeToError errorMessage maybeMatch
  where hasPlayer = (== Just player) . dataPlayer . snd
        errorMessage = "Position of " ++ show player ++ " not found!"


registerPlayer
    :: MonadState ServerData m
    => String -> (SkatStateForPlayer -> IO ()) -> m Player
registerPlayer name onReply = do
    players <- dataPlayers <$> get
    let Player maxId = if null players then Player 0 else fst . Map.findMax $ players
    let newPlayer = Player (maxId + 1)
    let record = PlayerData name Nothing onReply
    insert newPlayer record
    return newPlayer

unregisterPlayer
    :: (MonadState ServerData m, MonadError String m)
    => Player -> m ()
unregisterPlayer player = do
    record <- lookup player
    when (isJust $ dataLobby record) $ leaveLobby player
    delete player

createLobby
    :: (MonadState ServerData m, MonadError String m, MonadIO m) => m Lobby
createLobby = do
    Lobby maxId <- fst . Map.findMax . dataLobbies <$> get
    let newLobby = Lobby (maxId + 1)
    insert newLobby emptyLobbyData
    using newLobby newGame
    return newLobby

enterLobby
    :: (MonadState ServerData m, MonadError String m)
    => Player -> Lobby -> PlayerPosition -> m ()
enterLobby player lobby position = do
    playerRecord   <- lookup player
    positionRecord <- using lobby $ lookup position
    when (isJust $ dataLobby playerRecord) $
        throwError $ (show player) ++ " is already in a lobby!"
    when (isJust $ dataPlayer positionRecord) $
        throwError $ (show position) ++ " is already used!"
    insert player playerRecord{ dataLobby = Just lobby }
    using lobby $
        insert position positionRecord{ dataPlayer = Just player }

removePlayerFromLobby
    :: (MonadState LobbyData m, MonadError String m) => Player -> m ()
removePlayerFromLobby player = do
    position <- lookupPlayerPosition player
    adjust position (\record ->
        record{ dataPlayer = Nothing, dataResigned = False }
        )

leaveLobby :: (MonadState ServerData m, MonadError String m) => Player -> m ()
leaveLobby player = do
    lobby <- lookupLobby player
    using lobby $ removePlayerFromLobby player
    adjust player (\record -> record{ dataLobby = Nothing })

newGame :: (MonadState LobbyData m, MonadIO m) => m ()
newGame = do
    shuffled <- shuffle deck
    modify (\lobby ->
        lobby{ dataSkatState = initialStateFromDeck shuffled }
        )
    forM_ lobbyPositions (\position ->
            adjust position (\record -> record{ dataResigned  = False })
        )

checkRestartGame :: MonadState LobbyData m => m Bool
checkRestartGame = do
    records <- Map.elems . dataPositions <$> get
    let flags = map dataResigned . filter (isJust . dataPlayer) $ records
    return $ (not $ null flags) && (all id flags)

handlePlayerAction
   :: (MonadIO m, MonadState ServerData m, MonadError String m)
   => Player -> ReceivePacket -> m ()

handlePlayerAction player (MakeMove move) = do
    lobby <- lookupLobby player
    using lobby $ do
        position <- lookupPlayerPosition player
        state <- dataSkatState <$> get
        newState <- liftEither $ play state position move
        modify (\lobby -> lobby{ dataSkatState = newState })
    broadcastState lobby

handlePlayerAction player (SetName name) = do
    record <- lookup player
    insert player record{ dataName = name }
    case dataLobby record of
        Just lobby -> broadcastState lobby
        Nothing    -> return ()

handlePlayerAction player Resign = do
    lobby <- lookupLobby player
    using lobby $ do
        position <- lookupPlayerPosition player
        adjust position (\record -> record {dataResigned = True})
        needsRestart <- checkRestartGame
        when needsRestart newGame
    broadcastState lobby

handlePlayerAction client ShowCards = do
    throwError "ShowCards not implemented here" -- TODO

handlePlayerAction client (JoinLobby num position) = do
    enterLobby client (Lobby num) position
    broadcastState (Lobby num)

lobbyNameMap
    :: (MonadState ServerData m, MonadError String m)
    => Lobby -> m (Map.Map String String)
lobbyNameMap lobby = do
    players <- using lobby $
        catMaybes . map dataPlayer . Map.elems . dataPositions <$> get
    positionNames <- using lobby $
        map show <$> mapM lookupPlayerPosition players
    playerNames <- map dataName <$> mapM lookup players
    return $ Map.fromList $ zip positionNames playerNames

broadcastState
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => Lobby -> m ()
broadcastState lobby = do
    positionAssocs <- using lobby $ Map.assocs . dataPositions <$> get
    skatState <- using lobby $ dataSkatState <$> get

    let numResigned  = length . filter (dataResigned . snd) $ positionAssocs
    let showingCards = map playerPosition $ filter showsCards (players skatState)
    nameMap          <- lobbyNameMap lobby

    forM_ positionAssocs ( \(position, record) ->
        case dataPlayer record of
            Nothing     -> return ()
            Just player -> do
                playerRecord <- lookup player
                (liftIO . dataReply playerRecord)
                    (SkatStateForPlayer position skatState nameMap numResigned showingCards)
        )
