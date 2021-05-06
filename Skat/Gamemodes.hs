module Skat.Gamemodes(mRamsch, mGrand, mColor, mNull) where

import Skat.Definitions
import Data.Map
import Data.List
import Data.Ord
import Data.Maybe


sumCards :: [Card] -> Int
sumCards = sum . Data.List.map (\(Card name _) -> nameValue name)

simpleWinner :: Map PlayerPosition [Card] -> PlayerPosition
simpleWinner = fst . maximumBy (comparing (sumCards . snd)) . assocs

-- meine stiche
scoreSufficient :: SkatGewinnstufe -> [Card] -> Bool
scoreSufficient angesagt cards
    | angesagt == Normal    && (sumCards cards)  > 60   = True
    | angesagt == Schneider && (sumCards cards) >= 90  = True
    | angesagt == Schwarz   && (length   cards) == 32  = True
    | angesagt == Ouvert    && (length   cards) == 32  = True
    | otherwise                                        = False

singlePlayerHasWon :: PlayerPosition -> SkatScoringInformation -> Map PlayerPosition [Card] -> Bool
singlePlayerHasWon pos SkatScoringInformation{angesagteStufe=stufe} cards = scoreSufficient stufe (cards ! pos)


mRamsch :: GameMode
mRamsch = GameMode {
    cardsCompatible = simpleCompatible,
    cardSmaller = simpleCardLE,
    gameValue = \_ x -> sumCards x,
    determineGameWinner = \_ _ scores -> (simpleWinner scores, False),
    nicesShow = ("Ramsch", "")
}

-- spitzen, sorted. actual cards. count, present?
countSpitzen :: [Card] -> [Card] -> (Int, Bool)
countSpitzen []     _     = (0, False)
countSpitzen (x:xs) cards
    | (x `elem` cards) == nextPresent = (1 + nextCnt, x `elem` cards)
    | otherwise                       = (1, x `elem` cards)
    where otherSpitzen = countSpitzen xs cards
          nextPresent  = snd otherSpitzen
          nextCnt      = fst otherSpitzen

bubenSpitzen :: [Card]
bubenSpitzen = reverse $ sort [(Card Jack suit) | suit <- suits]

farbSpitzen :: Suit -> [Card]
farbSpitzen suit = bubenSpitzen ++ (reverse $ sort [(Card kind suit) | kind <- names, kind /= Jack])

-- spitzen, info, value
factorGameValue :: [Card] -> SkatScoringInformation -> Int
factorGameValue spitzen SkatScoringInformation{initialCards = cards} = fst $ countSpitzen spitzen cards

-- Achtung: Unterscheidet sich von der offiziellen Skatordnung, es kann auch ohne Hand angesagt werden!
-- angesagt, score of single player
factorGewinnstufe :: SkatScoringInformation -> [Card] -> Int
factorGewinnstufe state@SkatScoringInformation{angesagteStufe=stufe} myCards
    | stufe == Ouvert                    = 6 + handBonus -- ouvert
    | stufe == Schwarz                   = 5 + handBonus -- schwarz angesagt
    | stufe == Schneider && schwarz      = 4 + handBonus -- schneider angesagt
    | stufe == Schneider || schwarz      = 3 + handBonus -- schneider angesagt
    | schneider                          = 2 + handBonus -- schneider
    | stufe == Normal                    = 1 + handBonus -- normal
    where handBonus = if isHand state then 1 else 0
          schneider = sumCards myCards >= 90 || sumCards myCards <= 30
          schwarz   = length myCards == 32   || length myCards == 0

mGrand :: GameMode
mGrand = GameMode {
    cardsCompatible = simpleCompatible,
    cardSmaller = simpleCardLE,
    gameValue = \x cards -> 24 * ((factorGameValue bubenSpitzen x) + (factorGewinnstufe x cards)),
    determineGameWinner = \x info cards -> case x of
                                Just player -> (player, singlePlayerHasWon player info cards)
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
    gameValue = \x cards -> (suitValue color) * ((factorGameValue (farbSpitzen color) x) + (factorGewinnstufe x cards)),
    determineGameWinner = \x info cards -> case x of
                                Just player -> (player, singlePlayerHasWon player info cards)
                                _ -> error "Farb but nobody played??",
    nicesShow = ("Farbspiel", (show color))
}

nullCompatible :: Card -> Card -> Bool
nullCompatible (Card _ suit) (Card _ suit') = suit == suit'

nullOrdering :: [Name]
nullOrdering = [Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

nullCardLE :: Card -> Card -> Bool
nullCardLE (Card x suit) (Card y suit')
    | suit == suit' = fromJust (elemIndex x nullOrdering) <= fromJust (elemIndex y nullOrdering)
    | otherwise     = False

nullWinner :: PlayerPosition -> Map PlayerPosition [Card] -> Bool
nullWinner singlePlayer scores = length (scores ! singlePlayer) <= 3

nullValue :: SkatScoringInformation -> Int
nullValue SkatScoringInformation{isHand=True, angesagteStufe = Ouvert} = 59
nullValue SkatScoringInformation{angesagteStufe = Ouvert} = 46
nullValue SkatScoringInformation{isHand=True} = 35
nullValue _ = 23

mNull :: GameMode
mNull = GameMode {
    cardsCompatible = nullCompatible,
    cardSmaller = nullCardLE,
    gameValue = \x _ -> nullValue x,
    determineGameWinner = \x _ cards -> case x of
                                Just player -> (player, nullWinner player cards)
                                _ -> error "Null but nobody played??",
    nicesShow = ("Null", "")
}

{- ========= scoring ========== -}


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
