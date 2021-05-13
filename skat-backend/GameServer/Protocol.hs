module GameServer.Protocol
  ( CensoredCard(..)
  , PhaseInfo(..)
  , PublicInfo(..)
  , PrivateInfo(..)
  , PickingSubPhase(..)
  , LobbyInformation(..)
  , GameRequest(..)
  , GameResponse(..)
  )
where

import qualified Data.Map as Map

import Skat.Definitions hiding (Player, result, players)

data GameRequest
    = MakeMove SkatMove
    | SetName String
    | Resign
    | LeaveLobby
    | JoinLobby Int PlayerPosition
    | ChangePosition PlayerPosition
    | SpectateLobby Int
    | LeaveSpectate
    deriving (Show, Eq)


data CensoredCard
    = Censored
    | NotCensored Card
    deriving (Show, Eq, Ord)


data PublicInfo = PublicInfo
    { infoTurn        :: Maybe PlayerPosition
    , infoCards       :: Map.Map PlayerPosition [CensoredCard]
    , infoNames       :: Map.Map PlayerPosition String
    , infoNumResigned :: Int
    }

data PrivateInfo = PrivateInfo
    { infoYourPosition :: PlayerPosition
    , infoYourTurn     :: Bool
    , infoYourCards    :: [Card]
    , infoWonCards     :: [Card]
    , infoShowingCards :: Bool
    , infoResigned     :: Bool
    }

data PickingSubPhase = PickingHand | DiscardingSkat | PickingGamemode
    deriving (Eq, Ord, Show)

data PhaseInfo
    = ReizPhaseInfo
        { infoIsAnsagerTurn :: Bool
        , infoBid           :: Int
        }
    | PickingPhaseInfo
        { infoSubPhase       :: PickingSubPhase
        , infoPickingPlayer  :: PlayerPosition
        , infoCardsToDiscard :: Int
        , infoIsPlayingHand  :: Maybe Bool
        }
    | RunningPhaseInfo
        { infoGameMode     :: GameMode
        , infoScoring      :: SkatScoringInformation
        , infoCurrentStich :: Stich
        , infoLastStich    :: Stich
        , infoSinglePlayer :: Maybe PlayerPosition
        }
    | FinishedPhaseInfo
        { infoLastStich     :: Stich
        , infoScores        :: Map.Map PlayerPosition Int
        , infoScoringResult :: ScoringResult
        }

data LobbyInformation = LobbyInformation
    { infoLobbyId        :: Int
    , infoLobbyName      :: String
    , infoLobbyPositions :: Map.Map PlayerPosition String
    }

data GameResponse
    = StatePlayerResponse PhaseInfo PublicInfo PrivateInfo
    | StateSpectatorResponse PhaseInfo PublicInfo
    | LobbyResponse [LobbyInformation]
