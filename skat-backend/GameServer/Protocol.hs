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
    deriving (Show, Eq)


data CensoredCard = Censored | NotCensored Card


data PublicInfo = PublicInfo
  { pubTurn        :: Maybe PlayerPosition
  , pubCards       :: Map.Map PlayerPosition [CensoredCard]
  , pubNames       :: Map.Map PlayerPosition String
  , pubNumResigned :: Int
  }

data PrivateInfo = PrivateInfo
  { yourPosition :: PlayerPosition
  , yourTurn     :: Bool
  , yourCards    :: [Card]
  , wonCards     :: [Card]
  , resigned     :: Bool
  }

data PickingSubPhase = PickingHand | DiscardingSkat | PickingGamemode
    deriving (Eq, Ord, Show)

data PhaseInfo
    = ReizPhaseInfo
      { reizTurn :: PlayerPosition
      , reizBid  :: Int
      }
    | PickingPhaseInfo
      { subPhase       :: PickingSubPhase
      , pickingPlayer  :: PlayerPosition
      , cardsToDiscard :: Int
      , isPlayingHand  :: Maybe Bool
      }
    | RunningPhaseInfo
      { gameMode     :: GameMode
      , scoring      :: SkatScoringInformation
      , currentStich :: Stich
      , lastStich    :: Stich
      , singlePlayer :: Maybe PlayerPosition
      }
    | FinishedPhaseInfo
      { lastStich     :: Stich
      , scores        :: Map.Map PlayerPosition Int
      , scoringResult :: ScoringResult
      }

data LobbyInformation = LobbyInformation
  { lobbyId        :: Int
  , lobbyName      :: String
  , lobbyPositions :: Map.Map PlayerPosition String
  }

data GameResponse
    = StatePlayerResponse PhaseInfo PublicInfo PrivateInfo
    | StateSpectatorResponse PhaseInfo PublicInfo
    | LobbyResponse [LobbyInformation]
