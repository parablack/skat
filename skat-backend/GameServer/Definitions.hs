{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts       #-}

module GameServer.Definitions (
    User(..),
    UserData(..),
    Lobby(..),
    LobbyData(..),
    PositionData(..),
    ServerData(..),
    ServerRecord(..),
    emptyPositionData,
    emptyLobbyData,
    emptyServerData,
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy (MonadState, get, modify, StateT, runStateT)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (lookup)

import GameServer.Protocol

import Skat.Definitions
import Skat.Skat

newtype User = User String
    deriving (Show, Eq, Ord)

newtype Lobby = Lobby Int
    deriving (Show, Eq, Ord)

data UserData = UserData
    { dataUserName  :: String
    , dataLobby     :: Maybe Lobby
    , dataReply     :: GameResponse -> IO ()
    }

data LobbyData = LobbyData
    { dataPositions  :: Map.Map PlayerPosition PositionData
    , dataSkatState  :: SkatState
    , dataLobbyName  :: String
    , dataSpectators :: [User]
    }

data PositionData = PositionData
    { dataPlayer   :: Maybe User
    , dataResigned :: Bool
    }

data ServerData = ServerData
    { dataUsers   :: Map.Map User UserData
    , dataLobbies :: Map.Map Lobby LobbyData
    }

emptyPositionData :: PositionData
emptyPositionData = PositionData Nothing False

emptyLobbyData :: LobbyData
emptyLobbyData = LobbyData
    { dataPositions = Map.fromList [
        (Geber,      emptyPositionData),
        (Vorhand,    emptyPositionData),
        (Mittelhand, emptyPositionData)
        ]
    , dataSkatState  = initialStateFromDeck defaultDeck
    , dataLobbyName  = "[No Name]"
    , dataSpectators = []
    }

emptyServerData = ServerData Map.empty Map.empty
emptyServerData :: ServerData

class (Show i, Ord i) => ServerRecord i s r | i -> s, i -> r where
    extractMap :: s -> Map.Map i r
    modifyMap :: MonadState s m => (Map.Map i r -> Map.Map i r) -> m ()

    lookup :: (MonadState s m, MonadError String m) => i -> m r
    lookup key = do
        value <- Map.lookup key . extractMap <$> get
        case value of
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
            Right value  -> insert key newRecord >> return value


instance ServerRecord User ServerData UserData where
    extractMap = dataUsers
    modifyMap modifier = modify (\server ->
        server{ dataUsers = modifier (dataUsers server) })

instance ServerRecord Lobby ServerData LobbyData where
    extractMap = dataLobbies
    modifyMap modifier = modify (\server ->
        server{ dataLobbies = modifier (dataLobbies server) })

instance ServerRecord PlayerPosition LobbyData PositionData where
    extractMap = dataPositions
    modifyMap modifier = modify (\lobby ->
        lobby{ dataPositions = modifier (dataPositions lobby) })
