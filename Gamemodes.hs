module Gamemodes(mRamsch, mGrand, mColor, mNull) where

import Definitions
import Data.Map
import Data.List
import Data.Ord
import Data.Maybe


simpleWinner :: Map PlayerPosition Int -> PlayerPosition
simpleWinner = fst . maximumBy (comparing snd) . assocs

-- single player
simpleTeamWinner :: PlayerPosition -> Map PlayerPosition Int -> [PlayerPosition]
simpleTeamWinner singlePlayer scores
    | (scores ! singlePlayer) > 60 = [singlePlayer]
    | otherwise                    = [Vorhand, Geber, Mittelhand] Data.List.\\ [singlePlayer]


mRamsch = GameMode {
    cardsCompatible = simpleCompatible,
    cardSmaller = simpleCardLE,
    scoreMultiplier = 1,
    determineGameWinner = \_ -> (return . simpleWinner),
    nicesShow = ("Ramsch", "")
}

mGrand = GameMode {
    cardsCompatible = simpleCompatible,
    cardSmaller = simpleCardLE,
    scoreMultiplier = 24,
    determineGameWinner = \x -> case x of
                                Just player -> simpleTeamWinner player
                                _ -> error "Grand but nobody played??",
    nicesShow = ("Grand", "")
}

farbCompatible :: Suit -> Card -> Card -> Bool
farbCompatible _ (Card Jack _) (Card Jack _) = True
farbCompatible suit (Card Jack _) (Card _ s')
    | suit == s' = True
    | otherwise  = False
farbCompatible suit (Card _ s') (Card Jack _)
    | suit == s' = True
    | otherwise  = False
farbCompatible _ (Card _ c1) (Card _ c2) = c1 == c2

farbCardLE :: Suit -> Card -> Card -> Bool
farbCardLE _ (Card Jack suit) (Card Jack suit') = suit <= suit'
farbCardLE _ (Card Jack _) (Card _ _) = False
farbCardLE _ (Card _ _) (Card Jack _) = True
farbCardLE suit c1@(Card k1 s1) c2@(Card k2 s2)
    | suit == s2 && suit /= s1 = True
    | suit == s2 && suit == s1 = k1 <= k2
    | otherwise = simpleCardLE c1 c2


mColor :: Suit -> GameMode
mColor color = GameMode {
    cardsCompatible = farbCompatible color,
    cardSmaller = farbCardLE color,
    scoreMultiplier = suitValue color,
    determineGameWinner = \x -> case x of
                                Just player -> simpleTeamWinner player
                                _ -> error "Farb but nobody played??",
    nicesShow = ("Farbspiel", (show color))
}

nullCompatible :: Card -> Card -> Bool
nullCompatible (Card _ suit) (Card _ suit') = suit == suit'


nullOrdering = [Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
nullCardLE (Card x suit) (Card y suit')
    | suit == suit' = fromJust (elemIndex x nullOrdering) <= fromJust (elemIndex y nullOrdering)
    | otherwise     = False

-- TODO Stiche vs. Punkte
nullWinner :: PlayerPosition -> Map PlayerPosition Int -> [PlayerPosition]
nullWinner singlePlayer scores
    | (scores ! singlePlayer) == 0 = [singlePlayer]
    | otherwise                    = [Vorhand, Geber, Mittelhand] Data.List.\\ [singlePlayer]


mNull :: GameMode
mNull = GameMode {
    cardsCompatible = nullCompatible,
    cardSmaller = nullCardLE,
    scoreMultiplier = 23,
    determineGameWinner = \x -> case x of
                                Just player -> nullWinner player
                                _ -> error "Null but nobody played??",
    nicesShow = ("Null", "")
}



--canPlayRamsch :: SkatState -> PlayerPosition -> Card -> Bool
--canPlayRamsch state@RunningPhase{currentStich=currentStich} pos card = case currentStich of
--    [] -> True
--    lst -> let firstCardInStich = fst $ last lst
--               myCards          = playerCards (playerFromPos state pos)
--           in
--            -- alles easy, Karte passt
--        trivialCompatible firstCardInStich card ||
--            -- drauflegen falls keine Karte mehr dort ist
--        not $ any (trivialCompatible firstCardInStich) myCards
--canPlayRamsch _ _ _ = False
