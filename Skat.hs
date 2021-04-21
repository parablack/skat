module Skat(play, mRamsch) where
-- TODO mRamsch is unnec

import Control.Exception
import Data.List
import Definitions
import Ramsch

-- newtype GamemodeFarbspiel = GamemodeFarbspiel Suit
-- data GamemodeGrand =     GamemodeGrand
-- data GamemodeNull =      GamemodeNull


playerFromPos :: SkatState -> PlayerPosition -> Player
playerFromPos state pos = case find ((== pos) . playerPosition) (players state) of
    Nothing -> error $ "No player at position " ++ show pos ++ " found!"
    Just player -> player

-- for the current reizAnsager
maxReizValue :: SkatState -> Int
maxReizValue _ = 100 -- TODO

canReizen :: SkatState -> PlayerPosition -> Reizwert -> Bool
canReizen ReizPhase{hasReizAntwort=False} player value = False
canReizen state@ReizPhase{reizAnsager=ansager} player value
    | player == ansager    = case value of
        Weg -> True
        Reizwert wert -> wert > highBid && wert <= maxReiz
    | otherwise            = False
    where highBid = highestBid state
          maxReiz = maxReizValue state
canReizen _ _ _ = False

reizen :: SkatState -> PlayerPosition -> Reizwert -> SkatState
reizen state Mittelhand Weg = state {
    reizAnsager = Geber
}
reizen state Geber Weg = state {
    reizAnsager = Vorhand
}
reizen state Vorhand Weg = error "Ramsch not implemented yet!" -- TODO
reizen state pos (Reizwert x) = state {
    highestBid = x,
    currentWinner = Just pos,
    hasReizAntwort = False
}


playerHasCard :: SkatState -> PlayerPosition -> Card -> Bool
playerHasCard state pos card = card `elem` playerCards (playerFromPos state pos)

gamemodeAllowsCard :: SkatState -> PlayerPosition -> Card -> Bool
gamemodeAllowsCard state@RunningPhase{currentStich=stich, gameMode=gm} pos card = case stich of
    [] -> True
    lst -> let firstCardInStich = fst $ last lst
               myCards          = playerCards (playerFromPos state pos)
               compatCheck      = cardsCompatible gm
           in
            -- alles easy, Karte passt
        compatCheck firstCardInStich card
            -- drauflegen falls keine Karte mehr dort ist
         || not (any (compatCheck firstCardInStich) myCards)
gamemodeAllowsCard _ _ _ = False


myAssert :: Bool -> String -> Hopefully ()
myAssert True _ = Right ()
myAssert False s = Left s

determineWinner :: Stich -> (Card -> Card -> Bool) -> PlayerPosition
determineWinner stich le
    | not (firstcard `le` secondcard) && not (firstcard `le` thirdcard) = snd $ stich !! 2
    | (firstcard `le` secondcard) && not (secondcard `le` thirdcard)    = snd $ stich !! 1
    | (firstcard `le` thirdcard) && not (thirdcard `le` secondcard)     = snd $ head stich
    | otherwise = error "No winner could be determined, something is very wrong."
    where firstcard = fst $ stich !! 2
          secondcard = fst $ stich !! 1
          thirdcard = fst $ head stich

addWonCardsToPlayer :: [Player] -> PlayerPosition -> [Card] -> [Player]
addWonCardsToPlayer [] _ _ = []
addWonCardsToPlayer (x:xs) pos cards = if playerPosition x == pos then x {
            wonCards = cards ++ wonCards x
        } : xs
        else x : addWonCardsToPlayer xs pos cards

removePlayedCard :: Card -> [Player] -> [Player]
removePlayedCard card = map (deleteFun card)
    where   deleteFun :: Card -> Player -> Player
            deleteFun card player= player { playerCards = delete card (playerCards player) }

play :: SkatState -> PlayerPosition -> Card -> Hopefully SkatState
play state@RunningPhase{turn=whoseTurn, gameMode=gm, currentStich=oldStich} pos card = do
    myAssert (pos == whoseTurn) "It is not your turn!"
    myAssert (playerHasCard state pos card) "You don't have this card, you h4x0r!"
    myAssert (gamemodeAllowsCard state pos card) "This card is not compatible to the already played cards"
    let stich = (card, pos) : oldStich
    let newstate = state {
        players = removePlayedCard card $ players state
    }
    if length stich == 3 then
        let winner = determineWinner stich $ cardSmaller gm
            stichCards = map fst stich
        in
            return newstate {
                turn = winner,
                currentStich = [],
                players = addWonCardsToPlayer (players state) winner stichCards
            }
    else
        return newstate {
            turn = nextPos whoseTurn,
            currentStich = stich
        }



{-

    Server -> Client:
        Vor-Vorbereitung:
            Deine ID ist X
            Der Name von Spieler Y ist Z
        Vorbereitung:
            Spieler Y hat diese Karten
            Spieler X ist am Zug mit reizen
            Spieler X hat Z gereizt
            Spieler X hat das Reizen gewonnen
            Hier ist der Skat!

        Spielstart:
            Diese Variante + Farbe wird gespielt
            Y ist am Zug
            Y hat diese Karte gespielt
            Dieser Stich geht an Y
            Das Spiel ist zu Ende! {Ergebnis}

    Client -> Server:
        Vor-Vorbereitung:
            Mein Name ist X
        Vorbereitung:
            Ich reize Z
            Ich möchte den Skat sehen
            Ich will diese Karten in den Skat legen
            Ich spiele Farbe Z + Variante Y
        Spiel:
            Ich spiele die Karte Z

-}


