module SkatScoring(gameValue) where
-- TODO mRamsch is unnec

import Control.Monad.Except
import Data.List
import qualified Data.Map
import Definitions
import Skat
import Gamemodes
import Util

countSpitzen :: [Card] -> Int
countSpitzen

gameValue :: SkatScoringInformation -> Int
gameValue SkatNoScoring = 0
gameValue SkatScoringInformation{isHand=hand, angesagteStufe=stufe, initialCards=cards}