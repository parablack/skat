{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module GameServer.Server (
    registerLobby,
    registerPlayer,
    unregisterPlayer,
    handlePlayerAction,
    joinFreeLobby
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy (MonadState, get, modify)
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map  as Map
import Prelude hiding (lookup)

import Skat.Definitions hiding (Player, result, players)
import Skat.Skat
import GameServer.Protocol
import GameServer.Definitions
import Util


lookupLobby
    :: (MonadState ServerData m, MonadError String m) => Player -> m Lobby
lookupLobby player =
    dataLobby <$> lookup player
    >>= maybeToError (show player ++ " is not in a lobby!")

lookupPlayerPosition
    :: (MonadState LobbyData m, MonadError String m) => Player -> m PlayerPosition
lookupPlayerPosition player = do
    maybeMatch <- List.find hasPlayer . Map.assocs . dataPositions <$> get
    fst <$> maybeToError errorMessage maybeMatch
  where hasPlayer = (== Just player) . dataPlayer . snd
        errorMessage = "Position of " ++ show player ++ " not found!"

freePositions
    :: MonadState LobbyData m => m [PlayerPosition]
freePositions =
    map fst . filter (isNothing . dataPlayer . snd) . Map.assocs . dataPositions <$> get

addPlayerToLobby
    :: (MonadState ServerData m, MonadError String m)
    => Player -> Lobby -> PlayerPosition -> m ()
addPlayerToLobby player lobby position = do
    playerRecord   <- lookup player
    positionRecord <- using lobby $ lookup position
    when (isJust $ dataLobby playerRecord) $
        throwError $ (show player) ++ " is already in a lobby!"
    when (isJust $ dataPlayer positionRecord) $
        throwError $ (show position) ++ " is already used!"
    insert player playerRecord{ dataLobby = Just lobby }
    using lobby $
        insert position positionRecord{ dataPlayer = Just player }


removePlayerFromLobby
    :: (MonadState ServerData m, MonadError String m) => Player -> m ()
removePlayerFromLobby player = do
    lobby <- lookupLobby player
    using lobby $ do
        position <- lookupPlayerPosition player
        adjust position (\record ->
            record{ dataPlayer = Nothing, dataResigned = False }
            )
    adjust player (\record -> record{ dataLobby = Nothing })

newGame :: (MonadState LobbyData m, MonadIO m) => m ()
newGame = do
    shuffled <- shuffle deck
    modify (\lobby ->
        lobby{ dataSkatState = initialStateFromDeck shuffled }
        )
    forM_ allPlayerPositions (\position ->
            adjust position (\record -> record{ dataResigned  = False })
        )

changePositions
    :: MonadState LobbyData m
    => (PlayerPosition -> PlayerPosition) -> m ()
changePositions newPosition =
    modify (\record ->
        record{ dataPositions = Map.mapKeys newPosition (dataPositions record) }
        )

nextPosition :: PlayerPosition -> PlayerPosition
nextPosition Geber      = Mittelhand
nextPosition Mittelhand = Vorhand
nextPosition Vorhand    = Geber

rotatePositions :: MonadState LobbyData m => m ()
rotatePositions = changePositions nextPosition


checkRestartGame :: MonadState LobbyData m => m Bool
checkRestartGame = do
    records <- Map.elems . dataPositions <$> get
    let flags = map dataResigned . filter (isJust . dataPlayer) $ records
    return $ (not $ null flags) && (all id flags)

lobbyNameMap
    :: (MonadState ServerData m, MonadError String m)
    => Lobby -> m (Map.Map String String)
lobbyNameMap lobby = do
    players <- using lobby $
        catMaybes . map dataPlayer . Map.elems . dataPositions <$> get
    positionNames <- using lobby $
        map show <$> mapM lookupPlayerPosition players
    playerNames <- map dataPlayerName <$> mapM lookup players
    return $ Map.fromList $ zip positionNames playerNames

{- =============================================================== -}


buildPrivateInfo
    :: (MonadState ServerData m, MonadError String m) => m PrivateInfo
buildPrivateInfo player = do
    return PrivateInfo -- TODO
      { yourPosition = Geber
      , yourTurn     = False
      , yourCards    = []
      , wonCards     = []
      , resigned     = False
      }


censoredCards :: SkatState -> [PlayerPosition] -> Map PlayerPosition [CensoredCard]
censoredCards state showingCards =
    Map.fromList
        [ ("Geber",      censor Geber)
        , ("Vorhand",    censor Vorhand)
        , ("Mittelhand", censor Mittelhand)
        ]
    where
      censor position =
          let player = playerFromPos state position
              cards  = sort $ playerCards player
          in List.map (\card ->
                if position `elem` showingCards
                    then NotCensored card
                    else Censored
            ) cards

buildPublicInfo
    :: (MonadState ServerData m, MonadError String m) => Lobby -> m PublicInfo
buildPublicInfo lobby = do
    positionAssocs <- using lobby $ Map.assocs . dataPositions <$> get
    state   <- using lobby $ dataSkatState <$> get
    nameMap <- lobbyNameMap lobby
    let resigned = map fst . filter (dataResigned . snd) $ positionAssocs
    let showingCards = List.map playerPosition $ List.filter showsCards (players state)
    return PublicInfo
      { pubTurn        = Nothing                -- TODO
      , pubCards       = censoredCards state showingCards
      , pubNames       = nameMap
      , pubNumResigned = length resigned
      }

buildPhaseInfo :: SkatState -> PhaseInfo
buildPhaseInfo phase@ReizPhase{} = ReizPhaseInfo
    { reizTurn = reizAnsagerTurn state
    , reizBid  = reizCurrentBid state
    }
buildPhaseInfo phase@HandPickingPhase{} = PickingPhaseInfo
    { subPhase       = PickingHand
    , pickingPlayer  = singlePlayer phase
    , cardsToDiscard = (List.length . playerCards) (playerFromPos phase (singlePlayer phase)) - 10
    , isPlayingHand  = Nothing -- TODO
    }
buildPhaseInfo phase@SkatPickingPhase{} = PickingPhaseInfo
    { subPhase       = DiscardingSkat
    , pickingPlayer  = singlePlayer phase
    , cardsToDiscard = (List.length . playerCards) (playerFromPos phase (singlePlayer phase)) - 10
    , isPlayingHand  = Nothing -- TODO
    }
buildPhaseInfo phase@GamePickingPhase{} = PickingPhaseInfo
    { subPhase       = PickingGamemode
    , pickingPlayer  = singlePlayer phase
    , cardsToDiscard = (List.length . playerCards) (playerFromPos phase (singlePlayer phase)) - 10
    , isPlayingHand  = Nothing -- TODO
    }
buildPhaseInfo phase@RunningPhase{} = RunningPhaseInfo
    { gameMode     = gameMode phase
    , scoring      = skatScoringInformation phase
    , currentStich = List.reverse (currentStich phase)
    , lastStich    = case playedStiche state of
                        []    -> []
                        (x:_) -> Data.List.reverse x
    , singlePlayer = singlePlayer phase
    }
buildPhaseInfo phase@GameFinishedState{} = FinishedPhaseInfo
    { lastStich     = List.reverse (lastStich phase)
    , scores        = scores phase
    , scoringResult = result phase
    }


broadcastSkatState
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => Lobby -> m ()
broadcastSkatState lobby = do
    positionAssocs <- using lobby $ Map.assocs . dataPositions <$> get
    state <- using lobby $ dataSkatState <$> get
    let numResigned  = length . filter (dataResigned . snd) $ positionAssocs
    nameMap          <- lobbyNameMap lobby
    forM_ positionAssocs ( \(position, record) ->
        case dataPlayer record of
            Nothing     -> return ()
            Just player -> do
                playerRecord <- lookup player
                (liftIO . dataReply playerRecord)
                    (StateResponse (SkatStateForPlayer position state nameMap numResigned))
        )

buildLobbyInformation
    :: (MonadState ServerData m, MonadError String m)
    => Lobby -> m LobbyForPlayer
buildLobbyInformation lobby@(Lobby num) = do
    positions <- using lobby $ Map.map dataPlayer . dataPositions <$> get
    maybePosAssocs <-
        forM (Map.assocs positions) (\(position, maybePlayer) ->
                case maybePlayer of
                    Nothing -> return Nothing
                    Just player -> do
                        record <- lookup player
                        return $ Just (position, dataPlayerName record)
            )
    lobbyRecord <- lookup lobby
    return $ LobbyInformation {
        lobbyId        = num,
        lobbyName      = (dataLobbyName lobbyRecord),
        lobbyPositions = (Map.fromList . catMaybes $ maybePosAssocs)
    }

buildLobbyResponse
    :: (MonadState ServerData m, MonadError String m) => m GameResponse
buildLobbyResponse = do
    lobbies <- Map.keys . dataLobbies <$> get
    LobbyResponse <$> (sequence $ List.map buildLobbyInformation lobbies)

sendLobbies
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => Player -> m ()
sendLobbies player = do
    record <- lookup player
    buildLobbyResponse >>= liftIO . dataReply record

broadcastLobbies
    :: (MonadState ServerData m, MonadError String m, MonadIO m) => m ()
broadcastLobbies = do
    response <- buildLobbyResponse
    players <- Map.keys . dataPlayers <$> get
    forM_ players $ \player -> do
        record <- lookup player
        when (isNothing . dataLobby $ record) $
            liftIO . dataReply record $ response

{- =============================================================== -}

registerPlayer
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => Player -> String -> (GameResponse -> IO ()) -> m ()
registerPlayer player name onReply = do
    players <- Map.keys . dataPlayers <$> get
    if elem player players then
        throwError $ (show player) ++ " already registered!"
    else
        insert player (PlayerData name Nothing onReply)
    sendLobbies player

unregisterPlayer
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => Player -> m ()
unregisterPlayer player = do
    record <- lookup player
    case dataLobby record of
        Nothing -> return ()
        Just _  -> handlePlayerAction player LeaveLobby
    delete player

registerLobby
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => Lobby -> String -> m ()
registerLobby lobby name = do
    lobbies <- Map.keys . dataLobbies <$> get
    if elem lobby lobbies then
        throwError $ (show lobby) ++ " already registered!"
    else do
        insert lobby emptyLobbyData{ dataLobbyName = name }
        using lobby newGame
    broadcastLobbies


handlePlayerAction
   :: (MonadIO m, MonadState ServerData m, MonadError String m)
   => Player -> GameRequest -> m ()

handlePlayerAction player (MakeMove move) = do
    lobby <- lookupLobby player
    using lobby $ do
        position <- lookupPlayerPosition player
        state <- dataSkatState <$> get
        newState <- liftEither $ play state position move
        modify (\record -> record{ dataSkatState = newState })
    broadcastSkatState lobby

handlePlayerAction player (SetName name) = do
    record <- lookup player
    insert player record{ dataPlayerName = name }
    case dataLobby record of
        Just lobby -> broadcastSkatState lobby
        Nothing    -> return ()
    broadcastLobbies

handlePlayerAction player Resign = do
    lobby <- lookupLobby player
    needsRestart <- using lobby $ do
        position <- lookupPlayerPosition player
        adjust position (\record -> record {dataResigned = True})
        checkRestartGame
    when needsRestart $ do
        using lobby $ do
            newGame
            rotatePositions
        broadcastLobbies
    broadcastSkatState lobby

handlePlayerAction client (JoinLobby uid position) = do
    let lobby = Lobby uid
    addPlayerToLobby client lobby position
    players <- Map.keys . dataPlayers <$> get
    broadcastSkatState lobby
    broadcastLobbies

handlePlayerAction player LeaveLobby = do
    lobby <- lookupLobby player
    removePlayerFromLobby player
    broadcastSkatState lobby
    broadcastLobbies


handlePlayerAction player (ChangePosition newPlayerPosition) = do
    lobby <- lookupLobby player
    oldPlayerPosition <- using lobby $ lookupPlayerPosition player
    let newPosition position =
            if      position == newPlayerPosition then oldPlayerPosition
            else if position == oldPlayerPosition then newPlayerPosition
            else                                       position
    using lobby $ changePositions newPosition
    broadcastSkatState lobby
    broadcastLobbies


joinFreeLobby
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => Player -> m ()
joinFreeLobby player = do
    lobbies <- Map.keys . dataLobbies <$> get
    avail <- forM lobbies $ flip using freePositions
    let free = List.find (not . null . snd) $ zip lobbies avail
    case free of
        Just (lobby, position:_) -> do
            addPlayerToLobby player lobby position
            broadcastSkatState lobby
            broadcastLobbies
        _ -> throwError "No free lobby found!"
