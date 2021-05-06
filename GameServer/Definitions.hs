{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts       #-}

module GameServer.Definitions (
    ReceivePacket(..),
    SkatStateForPlayer(..),
    LobbyForPlayer(..),
    PlayerResponse(..),
    Player(..),
    PlayerData(..),
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

import Skat.Definitions hiding (Player, position, result, players, playerNames)
import Skat.Skat

newtype Player = Player String
    deriving (Show, Eq, Ord)

newtype Lobby = Lobby Int
    deriving (Show, Eq, Ord)

data ReceivePacket
    = MakeMove SkatMove
    | SetName String
    | Resign
    | LeaveLobby
    | JoinLobby Int PlayerPosition
    | ChangePosition PlayerPosition
    deriving (Show, Eq)


data SkatStateForPlayer = SkatStateForPlayer
  { position :: PlayerPosition,
    playerSkatState :: SkatState,
    playerNames :: Map.Map String String,
    resigningPlayers :: Int
}

data LobbyForPlayer = LobbyForPlayer
  { lobbyId        :: Int,
    lobbyName      :: String,
    lobbyPositions :: Map.Map PlayerPosition String
}

data PlayerResponse
    = StateResponse SkatStateForPlayer
    | LobbyResponse [LobbyForPlayer]

data PlayerData = PlayerData
  { dataPlayerName  :: String,
    dataLobby :: Maybe Lobby,
    dataReply :: PlayerResponse -> IO ()
  }

data LobbyData = LobbyData
  { dataPositions :: Map.Map PlayerPosition PositionData,
    dataSkatState :: SkatState,
    dataLobbyName :: String
  }

data PositionData = PositionData
  { dataPlayer    :: Maybe Player,
    dataResigned  :: Bool
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
    dataSkatState = initialStateFromDeck deck,
    dataLobbyName = "[No Name]"
  }

emptyServerData = ServerData Map.empty Map.empty
emptyServerData :: ServerData

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
