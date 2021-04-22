module Ramsch(mRamsch) where

import Definitions
import Data.Map
import Data.List
import Data.Ord

canonicalWinner :: Map PlayerPosition Int -> PlayerPosition
canonicalWinner = fst . maximumBy (comparing snd) . assocs

mRamsch = GameMode {
    cardsCompatible = simpleCompatible,
    cardSmaller = simpleCardLE,
    scoreMultiplier = 1,
    determineGameWinner = fst . minimumBy (comparing snd) . assocs,
    nicesShow = "Ramsch"
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
