module Skat.Skat(play, showCards, gameModeFromString, playerFromPos, playersFromDeck, initialStateFromDeck) where

import Control.Monad.Except
import Data.List
import qualified Data.Map
import Skat.Definitions
import Skat.Gamemodes
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
playersFromDeck deck = [Player Geber (slice 0 9 deck) [] False,
            Player Vorhand (slice 10 19 deck) [] False,
            Player Mittelhand (slice 20 29 deck) [] False
            ]

{- ================================================ -}

-- newtype GamemodeFarbspiel = GamemodeFarbspiel Suit
-- data GamemodeGrand =     GamemodeGrand
-- data GamemodeNull =      GamemodeNull

gameModeFromString :: String -> Hopefully GameMode
-- gameModeFromString "Ramsch" = mRamsch -- Cannot be chosen
gameModeFromString "Null" = return mNull
gameModeFromString "Grand" = return mGrand
gameModeFromString "ColorHearts" = return $ mColor Hearts
gameModeFromString "ColorSpades" = return $ mColor Spades
gameModeFromString "ColorDiamonds" = return $ mColor Diamonds
gameModeFromString "ColorClubs" = return $ mColor Clubs
gameModeFromString "Ramsch" = throwError "Ramsch cannot be actively played"
gameModeFromString _ = throwError "Invalid game variant. Do you even h4xx?"

skatFromDeck :: [Card] -> [Card]
skatFromDeck deck = slice 30 31 deck

initialStateFromDeck :: [Card] -> SkatState
initialStateFromDeck deck = ReizPhase {
        players = playersFromDeck deck,
        skat = skatFromDeck deck,
        reizStateMachine = MittelhandVorhand,
        reizAnsagerTurn = True,
        reizCurrentBid = 17
}

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
hasGameEnded RunningPhase{playedStiche = stiche} = length stiche == 10
hasGameEnded _     = False


checkGameEnd :: SkatState -> SkatState
checkGameEnd state = if hasGameEnded state then
    let cardsTuple = map (\x -> (playerPosition x, wonCards x)) $ players state
        cards =  foldl (\ls (player, score) -> Data.Map.insert player score ls) Data.Map.empty cardsTuple
        scoresTuple = map (\x -> (playerPosition x, scoreForPlayer x)) $ players state
        scores = foldl (\ls (player, score) -> Data.Map.insert player score ls) Data.Map.empty scoresTuple
        winner = determineGameWinner (gameMode state) (singlePlayer state) (skatScoringInformation state) cards
        winnerPlayer = playerFromPos state $ fst winner
        initialReizValue = reizHighestBid $ skatScoringInformation state
        value            = gameValue (gameMode state) (skatScoringInformation state) (wonCards winnerPlayer)
        ueberreizt       = initialReizValue > value
        hasWon           = snd winner && not ueberreizt
     in
    GameFinishedState {
        players = players state,
        lastStich = head $ playedStiche state,
        scores = scores,
        result = ScoringResult (fst winner) hasWon value ueberreizt,
        skatScoringInformation = skatScoringInformation state
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
    turn = Vorhand,
    skatScoringInformation = SkatScoringInformation {
        reizHighestBid = 0,
        isHand = False,
        angesagteStufe = Normal,
        initialCards = []
    }
}

-- player that won the reizen
handPickingFromReiz :: SkatState -> PlayerPosition -> SkatState
handPickingFromReiz state@ReizPhase{} singlePlayer = HandPickingPhase {
    players = players state,
    skat = skat state,
    reizCurrentBid = reizCurrentBid state,
    singlePlayer = Just singlePlayer
}
handPickingFromReiz _ _ = error "handPickingFromReiz called on non-reiz state."

{- ================ utils ============== -}
showCards :: SkatState -> PlayerPosition -> SkatState
showCards state pos = state {
    players = newPlayers
}
    where newPlayers = map (\x@Player{playerPosition=opos} -> if pos == opos then x { showsCards = True } else x) (players state)

{- ================= play phases ================== -}

play :: SkatState -> PlayerPosition -> SkatMove -> Hopefully SkatState

play state pos ShowCards = return $ showCards state pos

play state@RunningPhase{turn=whoseTurn} pos (PlayCard card) = do
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
        10 -> return GamePickingPhase {
            players = players newState,
            reizCurrentBid = reizCurrentBid newState,
            singlePlayer = Just singlePlayer,
            isHandSpiel = False
        }
        _  -> error "I am in SkatPicking state, but theres no Skat to Discard."

play state@ReizPhase{reizStateMachine=machine, reizAnsagerTurn=True} player (ReizBid val) = do
    assert (reizTurn state == Just player) "Du darfst aktuell kein Reizgebot abgeben!"
    let reizHighEnough = case val of
            Weg -> True
            Reizwert wert -> wert > (reizCurrentBid state)
    let reizLowEnough = case val of
            Weg -> True
            Reizwert wert -> wert <= 264

    assert (reizHighEnough) "Dein Gebot war zu billig! Gib dir mehr Mühe!"
    assert (reizLowEnough) "Dein Gebot war zu teuer! Gib dir weniger Mühe!"
    case val of
        Reizwert x ->
            if machine == VorhandNix then
                return $ handPickingFromReiz state Vorhand
            else return $ state {reizCurrentBid = x, reizAnsagerTurn = False}
        Weg        ->
            return $ case machine of
                VorhandNix -> ramschFromReiz state
                MittelhandVorhand -> state { reizStateMachine = GeberVorhand }
                GeberVorhand -> if reizCurrentBid state <= 17 then
                                    state { reizStateMachine = VorhandNix }
                                else handPickingFromReiz state Vorhand -- hat Vorhand schonmal ja gesagt --> Vorhand spielt. Sonst VorhandNix
                MittelhandGeber -> handPickingFromReiz state Geber

play ReizPhase{} Vorhand (ReizBid Weg) = throwError "pattern in Reizen does not match"


-- Bool: ja / nein?
play stateOrig@ReizPhase{reizStateMachine=machine, reizAnsagerTurn=False} player (ReizAnswer val) = do
    assert (reizTurn stateOrig == Just player) "Du darfst aktuell keine Reizantwort abgeben!"
    let state = stateOrig {reizAnsagerTurn = True}
    return $ case val of
        True -> state
        False -> case machine of
                VorhandNix -> error "Answer to VorhandNix, this should never happen"
                MittelhandVorhand -> stateOrig { reizStateMachine = MittelhandGeber }
                MittelhandGeber -> handPickingFromReiz state Mittelhand
                GeberVorhand -> handPickingFromReiz state Geber

play ReizPhase{} _ _ = throwError "pattern in reizAntwort does not match."

play state@HandPickingPhase{} pos (PlayHand var) = do
    assert (singlePlayer state == Just pos) "Du bist nicht dran mit Hand auswählen!"
    return $ case var of
        True ->
            -- Skat to Handplayer
            let  newPlayers = foldl (\players card -> (addWonCardsToPlayer pos [card] . removePlayedCard card) players) (players state) (skat state) in
            GamePickingPhase {
                players = newPlayers,
                reizCurrentBid = reizCurrentBid state,
                isHandSpiel = True,
                singlePlayer = singlePlayer state
            }
        False ->
            let newPlayers = addSkatToPlayerCard (skat state) pos (players state) in
            SkatPickingPhase {
                players = newPlayers,
                singlePlayer = singlePlayer state,
                reizCurrentBid = reizCurrentBid state
            }

play HandPickingPhase{} _ _ = throwError "Hand picking phase requires yes/no answer."

play state@GamePickingPhase{} pos (PlayVariant var angesagt) = do
    assert (singlePlayer state == Just pos) "Du bist nicht dran mit Spiel auswählen!"
    -- Achtung: Ansagen nur mit Hand?
    let playersOuvert = if angesagt == Ouvert then
            map (\player -> if playerPosition player == pos then player { showsCards = True } else player) (players state)
        else
            players state
    return $ RunningPhase {
        players = playersOuvert,
        singlePlayer = singlePlayer state,
        currentStich = [],
        playedStiche = [],
        turn = Vorhand,
        gameMode = var,
        skatScoringInformation = SkatScoringInformation {
            isHand = isHandSpiel state,
            angesagteStufe = angesagt,
            reizHighestBid = reizCurrentBid state,
            initialCards = playerCards (playerFromPos state pos) ++ wonCards (playerFromPos state pos)
        }
    }

play GamePickingPhase{} _ _ = throwError "Game picking phase requires a game pick, nothing else."

play _ _ _ = throwError "You are currently not allowed to make this move."
