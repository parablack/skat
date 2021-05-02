{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module SkatServer where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map  as Map
import Prelude hiding (lookup)

import Definitions
import Skat
import Util

newtype PlayerId = PlayerId Int
    deriving (Show, Eq, Ord)

newtype LobbyId = LobbyId Int
    deriving (Show, Eq, Ord)

data PlayerData = PlayerData
  { dataName  :: String,
    dataLobby :: Maybe LobbyId,
    dataReply :: SkatStateForPlayer -> IO (),
    dataError :: String -> IO ()
  }

data PositionData = PositionData
  { dataPlayer    :: Maybe PlayerId,
    dataResigned  :: Bool,
    dataShowCards :: Bool
  }

data LobbyData = LobbyData
  { dataPositions :: Map.Map PlayerPosition PositionData,
    dataSkatState :: SkatState
  }

data ServerData = ServerData
  { dataPlayers :: Map.Map PlayerId PlayerData,
    dataLobbies :: Map.Map LobbyId LobbyData
  }

emptyPositionData :: PositionData
emptyPositionData = PositionData Nothing False False

emptyLobbyData :: LobbyData
emptyLobbyData = LobbyData
  { dataPositions = Map.fromList [
        (Geber,      emptyPositionData),
        (Vorhand,    emptyPositionData),
        (Mittelhand, emptyPositionData)
        ],
    dataSkatState = initialStateFromDeck deck
  }

emptyServerData :: ServerData
emptyServerData = ServerData Map.empty Map.empty


class ServerRecord i s r | i -> s, i -> r where
    lookup :: (MonadState s m, MonadError String m) => i -> m r
    update :: (MonadState s m, MonadError String m) => i -> r -> m ()

    using :: (MonadState s m, MonadError String m)
          => i -> ExceptT String (StateT r m) a -> m a
    using recordId monad = do
        record <- lookup recordId
        (err, newRecord) <- runStateT (runExceptT monad) record
        case err of
            Left message -> throwError message
            Right result -> update recordId newRecord >> return result


errorLookup
    :: (MonadError String m, Show k, Ord k)
    => k -> Map.Map k a -> m a
errorLookup key map =
    case Map.lookup key map of
        Just val -> return val
        Nothing  -> throwError $ (show key) ++ " not found!"

instance ServerRecord PlayerId ServerData PlayerData where
    lookup playerId = get >>= errorLookup playerId . dataPlayers
    update playerId record = return ()

instance ServerRecord LobbyId ServerData LobbyData where
    lookup lobbyId = get >>= errorLookup lobbyId . dataLobbies
    update lobbyId record = return ()

instance ServerRecord PlayerPosition LobbyData PositionData where
    lookup position = get >>= errorLookup position . dataPositions
    update playerId record = return ()


registerPlayer
    :: MonadState ServerData m
    => String -> (SkatStateForPlayer -> IO ()) -> (String -> IO ()) -> m PlayerId
registerPlayer name onReply onError = do
    PlayerId maxId <- fst . Map.findMax . dataPlayers <$> get
    let newId = PlayerId (maxId + 1)
    let record = PlayerData name Nothing onReply onError
    -- TODO insert player
    return newId

-- unregisterPlayer
--    :: (MonadState ServerData m, MonadError String m)
--    => PlayerId -> m ()

enterLobby
    :: (MonadState ServerData m, MonadError String m)
    => PlayerId -> LobbyId -> PlayerPosition -> m ()
enterLobby playerId lobbyId position = do
    playerRecord   <- lookup playerId
    positionRecord <- using lobbyId $ lookup position
    when (isJust $ dataLobby playerRecord) $
        throwError $ (show playerId) ++ " is already in a lobby!"
    when (isJust $ dataPlayer positionRecord) $
        throwError $ (show position) ++ " is already used!"
    update playerId playerRecord{dataLobby = Just lobbyId}
    using lobbyId $ update position
        positionRecord{dataPlayer = Just playerId}

-- leaveLobby
--    :: (MonadState ServerData m, MonadError String m)
--    => PlayerId -> m ()

-- handlePlayerAction
--    :: (MonadIO m, MonadServerState m, MonadError String m)
--    => PlayerId -> ReceivePacket -> m ()
