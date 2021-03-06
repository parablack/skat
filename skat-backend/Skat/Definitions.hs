{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}


module Skat.Definitions(
     Suit(..), Name(..), Card(..), Player(..),
     Stich, Reizwert(..), GameMode(..), Hopefully,
     PlayerPosition(..), SkatState(..),
     ReizStateMachine(..),
     SkatMove(..),
     SkatScoringInformation(..),
     SkatGewinnstufe(..),
     ScoringResult(..),
     defaultDeck, nextPos, suits, names, nameValue, suitValue, simpleCompatible, simpleCardLE, activeReizPlayer, passiveReizPlayer, reizTurn,
     allPlayerPositions
) where

import Data.Maybe
import Data.List
import Data.Map
import Data.Aeson
import GHC.Generics

data Suit = Diamonds | Hearts | Clubs | Spades
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

suitValue :: Suit -> Int
suitValue Diamonds = 9
suitValue Hearts = 10
suitValue Spades = 11
suitValue Clubs = 12

instance (Ord Suit) where
    (<=) x y = suitValue x <= suitValue y


data Name = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

nameValue :: Name -> Int
nameValue Ten = 10
nameValue Ace = 11
nameValue Jack = 2
nameValue Queen = 3
nameValue King = 4
nameValue _ = 0

nameOrdering :: [Name]
nameOrdering = [Seven, Eight, Nine, Queen, King, Ten, Ace, Jack]

instance (Ord Name) where
    (<=) x y = fromJust (elemIndex x nameOrdering) <= fromJust (elemIndex y nameOrdering)



data Card = Card Name Suit
    deriving (Eq, Show)

instance (Ord Card) where
    (<=) = extendCardPartialOrder simpleCardLE

suits :: [Suit]
suits = [Clubs, Diamonds, Hearts, Spades]

names :: [Name]
names = [Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

defaultDeck :: [Card]
defaultDeck = [Card name suit | name <- names, suit <- suits]



data Player = Player {
    playerPosition :: PlayerPosition,
    playerCards :: [Card],
    wonCards :: [Card],
    showsCards :: Bool
} deriving (Eq, Show)

type Stich = [(Card, PlayerPosition)]

data Reizwert = Weg | Reizwert Int deriving (Eq, Show)

type Hopefully = Either String

type GameModeMeta = (String, String) -- description, suit

data GameMode = GameMode {
    cardsCompatible :: Card -> Card -> Bool,
    -- compare the two cards,
    cardSmaller :: Card -> Card -> Bool,
    -- single player, scores, (person, hasWon?)
    determineGameWinner :: Maybe PlayerPosition -> SkatScoringInformation -> (Map PlayerPosition [Card]) -> (PlayerPosition, Bool),
    -- scoring, cards of single player
    gameValue :: SkatScoringInformation -> [Card] -> Int,
    nicesShow :: GameModeMeta
}
instance (Show GameMode) where
    show x = (fst $ nicesShow x) ++ " " ++ (snd $ nicesShow x)
instance (Eq GameMode) where
    (==) x y = nicesShow x == nicesShow y

data SkatScoringInformation = SkatScoringInformation {
    isHand :: Bool,
    angesagteStufe :: SkatGewinnstufe,
    reizHighestBid :: Int,
    initialCards :: [Card]
} deriving (Eq, Show)

data SkatGewinnstufe = Normal | Schneider | Schwarz | Ouvert
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data PlayerPosition = Geber | Vorhand | Mittelhand
    deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON, ToJSONKey)

nextPos :: PlayerPosition -> PlayerPosition
nextPos Geber = Vorhand
nextPos Vorhand = Mittelhand
nextPos Mittelhand = Geber

allPlayerPositions :: [PlayerPosition]
allPlayerPositions = [Geber, Vorhand, Mittelhand]

data ScoringResult = ScoringResult
  { scoringPosition      :: PlayerPosition
  , scoringHasWon        :: Bool
  , scoringGameValue     :: Int
  , scoringHasUeberreizt :: Bool
  }
  deriving (Eq, Show)

data SkatState =
    ReizPhase {
        players :: [Player],
        skat :: [Card],
        reizStateMachine :: ReizStateMachine,
        reizAnsagerTurn :: Bool,
        reizCurrentBid :: Int
    } |
    HandPickingPhase {
        players :: [Player],
        skat :: [Card],
        reizCurrentBid :: Int,
        singlePlayer :: Maybe PlayerPosition
    } |
    SkatPickingPhase {
        players :: [Player],
        reizCurrentBid :: Int,
        singlePlayer :: Maybe PlayerPosition
    } |
    GamePickingPhase {
        players :: [Player],
        reizCurrentBid :: Int,
        isHandSpiel :: Bool,
        singlePlayer :: Maybe PlayerPosition
    } |
    RunningPhase {
        players :: [Player],
        singlePlayer :: Maybe PlayerPosition,
        gameMode :: GameMode,
        currentStich :: Stich,
        playedStiche :: [Stich],
        turn :: PlayerPosition,
        skatScoringInformation :: SkatScoringInformation
    } |
    GameFinishedState {
        players :: [Player],
        lastStich :: Stich,
        scores :: Map PlayerPosition Int,
        result :: ScoringResult,
        skatScoringInformation :: SkatScoringInformation
    }
    deriving (Eq, Show)


-- XY: X sagt, Y h??rt
data ReizStateMachine = MittelhandVorhand | MittelhandGeber | GeberVorhand | VorhandNix deriving(Eq, Show)
activeReizPlayer :: ReizStateMachine -> PlayerPosition
activeReizPlayer MittelhandVorhand = Mittelhand
activeReizPlayer MittelhandGeber = Mittelhand
activeReizPlayer GeberVorhand = Geber
activeReizPlayer VorhandNix = Vorhand

passiveReizPlayer :: ReizStateMachine -> Maybe PlayerPosition
passiveReizPlayer MittelhandVorhand = Just Vorhand
passiveReizPlayer MittelhandGeber = Just Geber
passiveReizPlayer GeberVorhand = Just Vorhand
passiveReizPlayer VorhandNix = Nothing

reizTurn :: SkatState -> Maybe PlayerPosition
reizTurn ReizPhase{reizAnsagerTurn=False, reizStateMachine=machine} = passiveReizPlayer machine
reizTurn ReizPhase{reizAnsagerTurn=True, reizStateMachine=machine} = return $ activeReizPlayer machine
reizTurn _ = Nothing

data SkatMove
    = PlayCard Card
    | PlayVariant GameMode SkatGewinnstufe
    | DiscardSkat Card Card
    | ReizBid Reizwert
    | ReizAnswer Bool
    | PlayHand Bool
    | ShowCards
    deriving (Show, Eq)


-- For Ramsch, grand
simpleCompatible :: Card -> Card -> Bool
simpleCompatible (Card Jack _) (Card Jack _) = True
simpleCompatible (Card Jack _) (Card _ _) = False
simpleCompatible (Card _ _) (Card Jack _) = False
simpleCompatible (Card _ c1) (Card _ c2) = c1 == c2

simpleCardLE :: Card -> Card -> Bool
simpleCardLE (Card Jack suit) (Card Jack suit') = suit <= suit'
simpleCardLE (Card Jack _) (Card _ _) = False
simpleCardLE (Card _ _) (Card Jack _) = True
simpleCardLE (Card kind suit) (Card kind' suit')
    | suit == suit' = kind <= kind'
    | otherwise     = False

extendCardPartialOrder :: (Card -> Card -> Bool) -> Card -> Card -> Bool
extendCardPartialOrder cmp c@(Card _ suit) c'@(Card _' suit')
        | cmp c c' = True
        | cmp c' c = False
        | otherwise         = suit <= suit'
