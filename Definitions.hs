{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}


module Definitions(Suit(..), Name(..), Card(..), Player(..),
     Stich(..), Reizwert(..), GameMode(..), Hopefully(..),
     PlayerPosition(..), SkatState(..), SkatStateForPlayer(..), ReceivePacket(..),
     ReizStateMachine(..),
     deck, nextPos, suits, names, nameValue, suitValue, simpleCompatible, simpleCardLE, activeReizPlayer) where

import Data.Maybe
import Data.List
import Data.Map
import Data.Aeson
import GHC.Generics

data Suit = Diamonds | Hearts | Clubs | Spades
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

suitValue Diamonds = 9
suitValue Hearts = 10
suitValue Spades = 11
suitValue Clubs = 12

instance (Ord Suit) where
    (<=) x y = suitValue x <= suitValue y


data Name = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

nameValue Ten = 10
nameValue Ace = 11
nameValue Jack = 2
nameValue Queen = 3
nameValue King = 4
nameValue _ = 0

nameOrdering = [Seven, Eight, Nine, Queen, King, Ten, Ace, Jack]

instance (Ord Name) where
    (<=) x y = fromJust (elemIndex x nameOrdering) <= fromJust (elemIndex y nameOrdering)



data Card = Card Name Suit
    deriving (Eq, Show)

instance (Ord Card) where
    (<=) = extendCardPartialOrder simpleCardLE

suits = [Clubs, Diamonds, Hearts, Spades]
names = [Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
deck = [Card name suit | name <- names, suit <- suits]



data Player = Player {
    playerPosition :: PlayerPosition,
    playerCards :: [Card],
    wonCards :: [Card]
} deriving (Eq, Show)

type Stich = [(Card, PlayerPosition)]

data Reizwert = Weg | Reizwert Int deriving (Eq, Show)

type Hopefully = Either String

data GameMode = GameMode {
    cardsCompatible :: Card -> Card -> Bool,
    -- compare the two cards,
    cardSmaller :: Card -> Card -> Bool,
    scoreMultiplier :: Int,
    determineGameWinner :: Map PlayerPosition Int -> PlayerPosition,
    nicesShow :: String
}
instance (Show GameMode) where
    show = nicesShow
instance (Eq GameMode) where
    (==) x y = nicesShow x == nicesShow y


data PlayerPosition = Geber | Vorhand | Mittelhand deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON, ToJSONKey)
nextPos :: PlayerPosition -> PlayerPosition
nextPos Geber = Vorhand
nextPos Vorhand = Mittelhand
nextPos Mittelhand = Geber

data SkatState =
    ReizPhase {
        players :: [Player],
        skat :: [Card],
        reizStateMachine :: ReizStateMachine,
        reizAnsagerTurn :: Bool,
        reizCurrentBid :: Int
    } |
    SkatPickingPhase {
        players :: [Player],
        skat :: [Card],
        singlePlayer :: Maybe PlayerPosition
    } |
    RunningPhase {
        players :: [Player],
        singlePlayer :: Maybe PlayerPosition,
        gameMode :: GameMode,
        currentStich :: Stich,
        playedStiche :: [Stich],
        turn :: PlayerPosition
    } |
    GameFinishedState {
        players :: [Player],
        lastStich :: Stich,
        scores :: Map PlayerPosition Int,
        winner :: PlayerPosition
    }
    deriving (Eq, Show)

data SkatStateForPlayer = SkatStateForPlayer {
    position :: PlayerPosition,
    playerSkatState :: SkatState,
    playerNames :: Map String String,
    resigningPlayers :: Int
}

-- XY: X sagt, Y hört
data ReizStateMachine = MittelhandVorhand | MittelhandGeber | GeberVorhand | VorhandNix deriving(Eq, Show)
activeReizPlayer :: ReizStateMachine -> PlayerPosition
activeReizPlayer MittelhandVorhand = Mittelhand
activeReizPlayer MittelhandGeber = Mittelhand
activeReizPlayer GeberVorhand = Geber
activeReizPlayer VorhandNix = Vorhand


data ReceivePacket = PlayCard Card | SetName String | PlayVariant GameMode | ShowCards | Resign | DiscardSkat Card Card deriving (Show, Eq)

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
extendCardPartialOrder cmp c@(Card name suit) c'@(Card name' suit')
        | cmp c c' = True
        | cmp c' c = False
        | otherwise         = suit <= suit'
