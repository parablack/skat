module Ramsch(mRamsch) where

import Definitions

mRamsch = GameMode {
    cardsCompatible = simpleCompatible,
    cardSmaller = simpleCardLE,
    scoreMultiplier = 1,
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
