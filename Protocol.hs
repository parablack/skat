import Control.Exception
import Data.List
import Definitions
import Ramsch

-- newtype GamemodeFarbspiel = GamemodeFarbspiel Suit
-- data GamemodeGrand =     GamemodeGrand
-- data GamemodeNull =      GamemodeNull


slice start end = take (end - start + 1) . drop start



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

myAssert :: Bool -> String -> Hopefully ()
myAssert True _ = Right ()
myAssert False s = Left s

play :: SkatState -> PlayerPosition -> Card -> Hopefully SkatState
play state@RunningPhase{currentStich = stich, turn=whoseTurn} pos card = do
    myAssert (pos == whoseTurn) "It is not your turn!"
    myAssert (playerHasCard state pos card) "You don't have this card, you h4x0r!"
    -- GameState play
    if length stich == 3 then
        return state {
            turn = nextPos whoseTurn -- TODO
        }
    else
        return state {
            turn = nextPos whoseTurn,
            currentStich = (card, pos) : stich
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


debugdeck = [Card name suit | name <- names, suit <- suits]
-- debugSkatState = ReizPhase {
--     players = [Player Geber (slice 0 9 debugdeck) [],
--                Player Vorhand (slice 10 19 debugdeck) [],
--                Player Mittelhand (slice 20 29 debugdeck) []
--               ],
--     skat = slice 30 31 debugdeck,
--     reizAnsager = Mittelhand,
--     reizHoerer = Vorhand,
--     hasReizAntwort = True,
--     highestBid = 0,
--     currentWinner = Nothing
-- }
debugRamschState = RunningPhase {
    players = [Player Geber (slice 0 9 debugdeck) [],
            Player Vorhand (slice 10 19 debugdeck) [],
            Player Mittelhand (slice 20 29 debugdeck) []
            ],
    singlePlayer = Nothing,
    gameMode = mRamsch,
    currentStich = [],
    turn = Vorhand
}
