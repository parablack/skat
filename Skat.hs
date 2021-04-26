module Skat(play, gameModeFromString, playerFromPos,mRamsch, ramschFromShuffledDeck, playersFromDeck, initialStateFromDeck) where
-- TODO mRamsch is unnec

import Control.Monad.Except
import Data.List
import qualified Data.Map
import Definitions
import Ramsch
import Util

{- ======== player util functions =============== -}

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

addWonCardsToPlayer :: PlayerPosition -> [Card] -> [Player] -> [Player]
addWonCardsToPlayer _ _ [] = []
addWonCardsToPlayer pos cards (x:xs) =
    if playerPosition x == pos
        then x {wonCards = cards ++ wonCards x} : xs
        else x : addWonCardsToPlayer pos cards xs

playerHasCard :: SkatState -> PlayerPosition -> Card -> Bool
playerHasCard state pos card = card `elem` playerCards (playerFromPos state pos)

removePlayedCard :: Card -> [Player] -> [Player]
removePlayedCard card = map (deleteFun card)
    where   deleteFun :: Card -> Player -> Player
            deleteFun card player= player { playerCards = delete card (playerCards player) }

scoreForPlayer :: Player -> Int
scoreForPlayer Player{wonCards=cards} = sum $ map (\(Card name _) -> nameValue name) cards

playersFromDeck :: [Card] -> [Player]
playersFromDeck deck = [Player Geber (slice 0 9 deck) [],
            Player Vorhand (slice 10 19 deck) [],
            Player Mittelhand (slice 20 29 deck) []
            ]

{- ================================================ -}

-- newtype GamemodeFarbspiel = GamemodeFarbspiel Suit
-- data GamemodeGrand =     GamemodeGrand
-- data GamemodeNull =      GamemodeNull

gameModeFromString :: String -> GameMode
gameModeFromString "Ramsch" = mRamsch

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

-- runningFromSkatPicking :: SkatState -> SkatState
-- runningFromSkatPicking state@SkatPickingPhase = RunningPhase {
--         players = players state,
--         singlePlayer = singlePlayer state,
--         gameMode :: GameMode,
--         currentStich :: Stich,
--         playedStiche :: [Stich],
--         turn :: PlayerPosition
--
-- } where newPlayers = addSkatToPlayerCard (skat state) singlePlayer (players state)
-- runningFromSkatPicking _ = error "runningFromSkatPicking called on non-picking state."


{- ========================= running phase utils ========================== -}

hasGameEnded :: SkatState -> Bool
hasGameEnded state@RunningPhase{playedStiche = stiche} = length stiche == 10
hasGameEnded _     = False


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
                players = addWonCardsToPlayer winner stichCards (players newstate)
            }
    | otherwise         = newstate {
            turn = nextPos whoseTurn,
            currentStich = stich
        }
    where stich    = (card, pos) : oldStich
          newstate = oldstate {
                        players = removePlayedCard card $ players oldstate
                     }

determineWinner :: Stich -> (Card -> Card -> Bool) -> PlayerPosition
determineWinner stich le
    | not (firstcard `le` secondcard) && not (firstcard `le` thirdcard) = snd $ stich !! 2
    | (firstcard `le` secondcard) && not (secondcard `le` thirdcard)    = snd $ stich !! 1
    | (firstcard `le` thirdcard) && not (thirdcard `le` secondcard)     = snd $ head stich
    | otherwise = error "No winner could be determined, something is very wrong."
  where firstcard = fst $ stich !! 2
        secondcard = fst $ stich !! 1
        thirdcard = fst $ head stich

gamemodeAllowsCard :: SkatState -> PlayerPosition -> Card -> Bool
gamemodeAllowsCard state@RunningPhase{currentStich=stich, gameMode=gm} pos card =
    case stich of
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


{- ========================= util reiz phase ======================= -}

ramschFromReiz :: SkatState -> SkatState
ramschFromReiz state = RunningPhase {
    players = players state,
    singlePlayer = Nothing,
    gameMode = mRamsch,
    currentStich = [],
    playedStiche = [],
    turn = Vorhand
}

-- player that won the reizen
skatPickingFromReiz :: SkatState -> PlayerPosition -> SkatState
skatPickingFromReiz state@ReizPhase{} singlePlayer = SkatPickingPhase {
    players = newPlayers,
    reizCurrentBid = reizCurrentBid state,
    singlePlayer = Just singlePlayer
} where newPlayers = addSkatToPlayerCard (skat state) singlePlayer (players state)
skatPickingFromReiz _ _ = error "SkatPickingFromReiz called on non-reiz state."

{- ================= play phases ================== -}

play :: SkatState -> PlayerPosition -> SkatMove -> Hopefully SkatState

play state@RunningPhase{turn=whoseTurn, gameMode=gm, currentStich=oldStich} pos (PlayCard card) = do
    assert (pos == whoseTurn) "It is not your turn!"
    assert (playerHasCard state pos card) "You don't have this card, you h4x0r!"
    assert (gamemodeAllowsCard state pos card) "This card is not compatible to the already played cards"
    return . checkGameEnd . playRawOnState state pos $ card

play state@SkatPickingPhase{singlePlayer=Just singlePlayer} pos (PlayCard card) = do
    assert (pos == singlePlayer) "Du spielst nicht alleine. Lass den Skat gefälligst in Ruhe!"
    assert (playerHasCard state pos card) "You don't have this card, you h4x0r!"
    let player = playerFromPos state singlePlayer
    let playerAmountCards = (length . playerCards) player
    when (playerAmountCards <= 10) $ return (error "Skat discarding but less than 11 cards left")
    -- let newPlayers = removePlayedCard card) $ players state
    let newState = state {
        players = (addWonCardsToPlayer pos [card] . removePlayedCard card) $ players state
    }
    case playerAmountCards - 1 of
        11 -> return newState
        10 -> return newState -- TODO
        _  -> error "I am in SkatPicking state, but theres no Skat to Discard."

play state@ReizPhase{reizStateMachine=machine, reizAnsagerTurn=True} player (ReizBid val) = do
    assert (reizTurn state == Just player) "Du darfst aktuell kein Reizgebot abgeben!"
    let reizHighEnough = case val of
            Weg -> True
            Reizwert wert -> wert > (reizCurrentBid state)
    assert (reizHighEnough) "Dein Gebot war zu billig! Gib dir mehr Mühe!"
    case val of
        Reizwert x ->
            if machine == VorhandNix then
                return $ skatPickingFromReiz state Vorhand
            else return $ state {reizCurrentBid = x, reizAnsagerTurn = False}
        Weg        ->
            return $ case machine of
                VorhandNix -> ramschFromReiz state
                MittelhandVorhand -> state { reizStateMachine = GeberVorhand }
                GeberVorhand -> if reizCurrentBid state == 0 then
                                    state { reizStateMachine = VorhandNix }
                                else skatPickingFromReiz state Vorhand -- hat Vorhand schonmal ja gesagt --> Vorhand spielt. Sonst VorhandNix
                MittelhandGeber -> skatPickingFromReiz state Geber

play state@ReizPhase{} Vorhand (ReizBid Weg) = throwError "pattern in Reizen does not match"


-- Bool: ja / nein?
play stateOrig@ReizPhase{reizStateMachine=machine, reizAnsagerTurn=False} player (ReizAnswer val) = do
    assert (reizTurn stateOrig == Just player) "Du darfst aktuell keine Reizantwort abgeben!"
    let state = stateOrig {reizAnsagerTurn = True}
    return $ case val of
        True -> state
        False -> case machine of
                VorhandNix -> error "Answer to VorhandNix, this should never happen"
                MittelhandVorhand -> state { reizStateMachine = MittelhandGeber }
                MittelhandGeber -> skatPickingFromReiz state Mittelhand
                GeberVorhand -> skatPickingFromReiz state Geber

play ReizPhase{} _ _ = throwError "pattern in reizAntwort does not match."

play _ _ _ = throwError "You are currently not allowed to make this move."
