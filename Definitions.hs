module Definitions(Suit(..), Name(..), Card(..), Player(..),
     Stich(..), Reizwert(..), GameMode(..), Hopefully(..),
     PlayerPosition(..), SkatState(..), nextPos, suits, names, simpleCompatible, simpleCardLE) where

import Data.Maybe
import Data.List

data Suit = Diamonds | Hearts | Clubs | Spades
    deriving (Eq, Show)

suitValue Diamonds = 9
suitValue Hearts = 10
suitValue Clubs = 11
suitValue Spades = 12

instance (Ord Suit) where
    (<=) x y = suitValue x <= suitValue y


data Name = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Show)

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
    deriving (Eq, Ord, Show)

suits = [Clubs, Diamonds, Hearts, Spades]
names = [Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]



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
    nicesShow :: String
}
instance (Show GameMode) where
    show = nicesShow
instance (Eq GameMode) where
    (==) x y = nicesShow x == nicesShow y


data PlayerPosition = Geber | Vorhand | Mittelhand deriving (Eq, Show, Ord)
nextPos :: PlayerPosition -> PlayerPosition
nextPos Geber = Vorhand
nextPos Vorhand = Mittelhand
nextPos Mittelhand = Geber

data SkatState =
    ReizPhase {
        players :: [Player],
        skat :: [Card],
        reizAnsager :: PlayerPosition,
        reizHoerer :: PlayerPosition,
        hasReizAntwort :: Bool,
        highestBid :: Int,
        currentWinner :: Maybe PlayerPosition
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
        turn :: PlayerPosition
    } deriving (Eq, Show)

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

