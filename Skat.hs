module Skat(play, gameModeFromString, playerFromPos,mRamsch, ramschFromShuffledDeck, playersFromDeck, reizen, reizenAntwort, initialStateFromDeck) where
-- TODO mRamsch is unnec

import Control.Monad.Except
import Data.List
import qualified Data.Map
import Definitions
import Ramsch

-- newtype GamemodeFarbspiel = GamemodeFarbspiel Suit
-- data GamemodeGrand =     GamemodeGrand
-- data GamemodeNull =      GamemodeNull


gameModeFromString :: String -> GameMode
gameModeFromString "Ramsch" = mRamsch

playerFromPos :: SkatState -> PlayerPosition -> Player
playerFromPos state pos = case find ((== pos) . playerPosition) (players state) of
    Nothing -> error $ "No player at position " ++ show pos ++ " found!"
    Just player -> player

-- skat, player that won reizen, player
addSkatToPlayerCard :: [Card] -> PlayerPosition -> [Player] -> [Player]
addSkatToPlayerCard skat winnerpos = map (addFun skat winnerpos)
    where   addFun :: [Card] -> PlayerPosition -> Player -> Player
            addFun skat winner player
                | playerPosition player == winner = player { playerCards = skat ++ (playerCards player) }
                | otherwise = player

-- player that won the reizen
skatPickingFromReiz :: SkatState -> PlayerPosition -> SkatState
skatPickingFromReiz state@ReizPhase{} singlePlayer = SkatPickingPhase {
    players = newPlayers,
    reizCurrentBid = reizCurrentBid state,
    singlePlayer = Just singlePlayer
} where newPlayers = addSkatToPlayerCard (skat state) singlePlayer (players state)
skatPickingFromReiz _ _ = error "SkatPickingFromReiz called on non-reiz state."

ramschFromReiz :: SkatState -> SkatState
ramschFromReiz state = RunningPhase {
    players = players state,
    singlePlayer = Nothing,
    gameMode = mRamsch,
    currentStich = [],
    playedStiche = [],
    turn = Vorhand
}

reizen :: SkatState -> PlayerPosition -> Reizwert -> Hopefully SkatState
reizen state@ReizPhase{reizStateMachine=machine, reizAnsagerTurn=True} player val = do
    myAssert (reizTurn state == Just player) "Du darfst aktuell kein Reizgebot abgeben!"
    let reizHighEnough = case val of
            Weg -> True
            Reizwert wert -> wert > (reizCurrentBid state)
    myAssert (reizHighEnough) "Dein Gebot war zu billig! Gib dir mehr Mühe!"
    let state2 = state {reizAnsagerTurn = False}
    case val of
        Reizwert x ->
            if machine == VorhandNix then
                return $ skatPickingFromReiz state2 Vorhand
            else return $ state2{reizCurrentBid = x}
        Weg        ->
            return $ case machine of
                VorhandNix -> ramschFromReiz state2
                MittelhandVorhand -> state2 { reizStateMachine = GeberVorhand }
                GeberVorhand -> if reizCurrentBid state2 == 0 then
                                    state2 { reizStateMachine = VorhandNix }
                                else skatPickingFromReiz state2 Vorhand -- hat Vorhand schonmal ja gesagt --> Vorhand spielt. Sonst VorhandNix
                MittelhandGeber -> skatPickingFromReiz state2 Geber
reizen state Vorhand Weg = throwError "pattern in Reizen does not match"

-- Bool: ja / nein?
reizenAntwort :: SkatState -> PlayerPosition -> Bool -> Hopefully SkatState
reizenAntwort state@ReizPhase{reizStateMachine=machine, reizAnsagerTurn=False} player val = do
    myAssert (reizTurn state == Just player) "Du darfst aktuell keine Reizantwort abgeben!"
    let state = state {reizAnsagerTurn = True}
    return $ case val of
        True -> state
        False -> case machine of
                VorhandNix -> error "Answer to VorhandNix, this should never happen"
                MittelhandVorhand -> state { reizStateMachine = MittelhandGeber }
                MittelhandGeber -> skatPickingFromReiz state Mittelhand
                GeberVorhand -> skatPickingFromReiz state Geber
reizenAntwort _ _ _ = Left "pattern in reizAntwort does not match."

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
myAssert True _ = return ()
myAssert False s = throwError s

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

hasGameEnded :: SkatState -> Bool
hasGameEnded state@RunningPhase{playedStiche = stiche} = length stiche == 10
hasGameEnded _     = False

scoreForPlayer :: Player -> Int
scoreForPlayer Player{wonCards=cards} = sum $ map (\(Card name _) -> nameValue name) cards

checkGameEnd :: SkatState -> SkatState
checkGameEnd state = if hasGameEnded state then
    let scoresTuple = map (\x -> (playerPosition x, scoreForPlayer x)) $ players state
        scores = foldl (\ls (player, score) -> Data.Map.insert player score ls) Data.Map.empty scoresTuple
        winner = determineGameWinner (gameMode state) scores
     in
    GameFinishedState {
        players = players state,
        lastStich = head $ playedStiche state,
        scores = scores,
        winner = winner
    }
    else state

playRawOnState :: SkatState -> PlayerPosition -> Card -> SkatState
playRawOnState oldstate@RunningPhase{turn=whoseTurn, gameMode=gm, currentStich=oldStich} pos card
    | length stich == 3 =
        let winner = determineWinner stich $ cardSmaller gm
            stichCards = map fst stich
        in
            newstate {
                turn = winner,
                currentStich = [],
                playedStiche = stich : playedStiche newstate,
                players = addWonCardsToPlayer (players newstate) winner stichCards
            }
    | otherwise         = newstate {
            turn = nextPos whoseTurn,
            currentStich = stich
        }
    where stich    = (card, pos) : oldStich
          newstate = oldstate {
                        players = removePlayedCard card $ players oldstate
                     }

play :: SkatState -> PlayerPosition -> Card -> Hopefully SkatState
play state@RunningPhase{turn=whoseTurn, gameMode=gm, currentStich=oldStich} pos card = do
    myAssert (pos == whoseTurn) "It is not your turn!"
    myAssert (playerHasCard state pos card) "You don't have this card, you h4x0r!"
    myAssert (gamemodeAllowsCard state pos card) "This card is not compatible to the already played cards"
    let newstate = playRawOnState state pos card
        newstateChecked = checkGameEnd newstate
    return newstateChecked

slice start end = take (end - start + 1) . drop start

playersFromDeck :: [Card] -> [Player]
playersFromDeck deck = [Player Geber (slice 0 9 deck) [],
            Player Vorhand (slice 10 19 deck) [],
            Player Mittelhand (slice 20 29 deck) []
            ]

skatFromDeck :: [Card] -> [Card]
skatFromDeck deck = slice 30 31 deck

initialStateFromDeck :: [Card] -> SkatState
initialStateFromDeck deck = ReizPhase {
        players = playersFromDeck deck,
        skat = skatFromDeck deck,
        reizStateMachine = MittelhandVorhand,
        reizAnsagerTurn = True,
        reizCurrentBid = 0
}

ramschFromShuffledDeck :: [Card] -> SkatState
ramschFromShuffledDeck deck = RunningPhase {
    players = playersFromDeck deck,
    singlePlayer = Nothing,
    gameMode = mRamsch,
    currentStich = [],
    playedStiche = [],
    turn = Vorhand
}

debugRamschState = ramschFromShuffledDeck deck
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
