{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module GameServer.Server (
    registerLobby,
    registerUser,
    unregisterUser,
    handleUserAction,
    joinFreeLobby
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy (MonadState, get, modify)
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map  as Map
import Prelude hiding (lookup)

import Skat.Definitions hiding (Player)
import Skat.Skat
import GameServer.Protocol
import GameServer.Definitions
import Util


lookupLobby
    :: (MonadState ServerData m, MonadError String m) => User -> m Lobby
lookupLobby user =
    dataLobby <$> lookup user
    >>= maybeToError (show user ++ " is not in a lobby!")

lookupPlayerPosition
    :: (MonadState LobbyData m, MonadError String m) => User -> m PlayerPosition
lookupPlayerPosition user = do
    maybeMatch <- List.find hasPlayer . Map.assocs . dataPositions <$> get
    fst <$> maybeToError errorMessage maybeMatch
  where hasPlayer = (== Just user) . dataPlayer . snd
        errorMessage = "Player position of " ++ show user ++ " not found!"

freePositions
    :: MonadState LobbyData m => m [PlayerPosition]
freePositions =
    map fst . filter (isNothing . dataPlayer . snd) . Map.assocs . dataPositions <$> get

addPlayerToLobby
    :: (MonadState ServerData m, MonadError String m)
    => User -> Lobby -> PlayerPosition -> m ()
addPlayerToLobby user lobby position = do
    userRecord     <- lookup user
    positionRecord <- using lobby $ lookup position
    when (isJust $ dataLobby userRecord) $
        throwError $ (show user) ++ " is already in a lobby!"
    when (isJust $ dataPlayer positionRecord) $
        throwError $ (show position) ++ " is already used!"
    insert user userRecord {dataLobby = Just lobby}
    using lobby $
        insert position positionRecord {dataPlayer = Just user}


removePlayerFromLobby
    :: (MonadState ServerData m, MonadError String m) => User -> m ()
removePlayerFromLobby user = do
    lobby <- lookupLobby user
    using lobby $ do
        position <- lookupPlayerPosition user
        adjust position $ \record ->
            record {dataPlayer = Nothing, dataResigned = False}
    adjust user $ \record -> record {dataLobby = Nothing}

newGame :: (MonadState LobbyData m, MonadIO m) => m ()
newGame = do
    shuffled <- shuffle defaultDeck
    modify $ \lobby ->
        lobby {dataSkatState = initialStateFromDeck shuffled}
    forM_ allPlayerPositions $ \position ->
        adjust position (\record -> record {dataResigned  = False})

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
    => Lobby -> m (Map.Map PlayerPosition String)
lobbyNameMap lobby = do
    users <- using lobby $
        catMaybes . map dataPlayer . Map.elems . dataPositions <$> get
    positionNames <- using lobby $ mapM lookupPlayerPosition users
    playerNames <- map dataUserName <$> mapM lookup users
    return $ Map.fromList $ zip positionNames playerNames

{- =============================================================== -}


currentTurn :: SkatState -> Maybe PlayerPosition
currentTurn state@ReizPhase{}         = reizTurn state
currentTurn state@SkatPickingPhase{}  = singlePlayer state
currentTurn state@GamePickingPhase{}  = singlePlayer state
currentTurn state@HandPickingPhase{}  = singlePlayer state
currentTurn state@RunningPhase{}      = Just $ turn state
currentTurn       GameFinishedState{} = Nothing

compareByMode :: SkatState -> Card -> Card -> Ordering
compareByMode state a b
    | (cmpLE a b) && (cmpLE b a) = EQ
    | cmpLE a b                  = LT
    | otherwise                  = GT
  where
    cmpLE = case state of
        phase@RunningPhase{} -> cardSmaller $ gameMode phase
        _                    -> simpleCardLE

buildPrivateInfo
    :: (MonadState ServerData m, MonadError String m) => User -> m PrivateInfo
buildPrivateInfo user = do
    lobby    <- lookupLobby user
    position <- using lobby $ lookupPlayerPosition user
    state    <- using lobby $ dataSkatState <$> get
    resigned <- using lobby $ dataResigned <$> lookup position
    let player = playerFromPos state position
    return PrivateInfo
        { infoYourPosition = position
        , infoYourTurn     = (currentTurn state == Just position)
        , infoYourCards    = playerCards player
        , infoWonCards     = List.sortBy (compareByMode state) $ wonCards player
        , infoShowingCards = showsCards player
        , infoResigned     = resigned
        }

censoredCards :: SkatState -> [PlayerPosition] -> Map.Map PlayerPosition [CensoredCard]
censoredCards state showingCards =
    Map.fromList
        [ (Geber,      censor Geber)
        , (Vorhand,    censor Vorhand)
        , (Mittelhand, censor Mittelhand)
        ]
  where
    censor :: PlayerPosition -> [CensoredCard]
    censor position =
        let player = playerFromPos state position
            cards  = List.sortBy (compareByMode state) $ playerCards player
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
        { infoTurn        = currentTurn state
        , infoCards       = censoredCards state showingCards
        , infoNames       = nameMap
        , infoNumResigned = length resigned
        }

buildPhaseInfo :: SkatState -> PhaseInfo
buildPhaseInfo phase@ReizPhase{} = ReizPhaseInfo
    { infoIsAnsagerTurn = reizAnsagerTurn phase
    , infoBid           = reizCurrentBid phase
    }

buildPhaseInfo phase@HandPickingPhase{} = PickingPhaseInfo
    { infoSubPhase       = PickingHand
    , infoPickingPlayer  = fromJust (singlePlayer phase)
    , infoCardsToDiscard = (List.length . playerCards) (playerFromPos phase (fromJust $ singlePlayer phase)) - 10
    , infoIsPlayingHand  = Nothing -- TODO
    }
buildPhaseInfo phase@SkatPickingPhase{} = PickingPhaseInfo
    { infoSubPhase       = DiscardingSkat
    , infoPickingPlayer  = fromJust (singlePlayer phase)
    , infoCardsToDiscard = (List.length . playerCards) (playerFromPos phase (fromJust $ singlePlayer phase)) - 10
    , infoIsPlayingHand  = Nothing -- TODO
    }
buildPhaseInfo phase@GamePickingPhase{} = PickingPhaseInfo
    { infoSubPhase       = PickingGamemode
    , infoPickingPlayer  = fromJust (singlePlayer phase)
    , infoCardsToDiscard = (List.length . playerCards) (playerFromPos phase (fromJust $ singlePlayer phase)) - 10
    , infoIsPlayingHand  = Nothing -- TODO
    }

buildPhaseInfo phase@RunningPhase{} = RunningPhaseInfo
    { infoGameMode     = gameMode phase
    , infoScoring      = skatScoringInformation phase
    , infoCurrentStich = List.reverse (currentStich phase)
    , infoLastStich    = case playedStiche phase of
                            []    -> []
                            (x:_) -> List.reverse x
    , infoSinglePlayer = singlePlayer phase
    }

buildPhaseInfo phase@GameFinishedState{} = FinishedPhaseInfo
    { infoLastStich     = List.reverse (lastStich phase)
    , infoScores        = scores phase
    , infoScoringResult = result phase
    }


broadcastSkatState
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => Lobby -> m ()
broadcastSkatState lobby = do
    publicInfo <- buildPublicInfo lobby
    state <- using lobby $ dataSkatState <$> get
    let phaseInfo = buildPhaseInfo state
    positionRecords <- using lobby $ Map.elems . dataPositions <$> get
    forM_ positionRecords $ \record ->
        case dataPlayer record of
            Nothing     -> return ()
            Just player -> do
                privateInfo <- buildPrivateInfo player
                playerRecord <- lookup player
                liftIO . dataReply playerRecord $
                    StatePlayerResponse phaseInfo publicInfo privateInfo

buildLobbyInformation
    :: (MonadState ServerData m, MonadError String m)
    => Lobby -> m LobbyInformation
buildLobbyInformation lobby@(Lobby num) = do
    positions <- using lobby $ Map.map dataPlayer . dataPositions <$> get
    maybePosAssocs <-
        forM (Map.assocs positions) (\(position, maybePlayer) ->
                case maybePlayer of
                    Nothing -> return Nothing
                    Just player -> do
                        record <- lookup player
                        return $ Just (position, dataUserName record)
            )
    lobbyRecord <- lookup lobby
    return $ LobbyInformation {
        infoLobbyId        = num,
        infoLobbyName      = (dataLobbyName lobbyRecord),
        infoLobbyPositions = (Map.fromList . catMaybes $ maybePosAssocs)
    }

buildLobbyResponse
    :: (MonadState ServerData m, MonadError String m) => m GameResponse
buildLobbyResponse = do
    lobbies <- Map.keys . dataLobbies <$> get
    LobbyResponse <$> (sequence $ List.map buildLobbyInformation lobbies)

sendLobbies
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => User -> m ()
sendLobbies user = do
    record <- lookup user
    buildLobbyResponse >>= liftIO . dataReply record

broadcastLobbies
    :: (MonadState ServerData m, MonadError String m, MonadIO m) => m ()
broadcastLobbies = do
    response <- buildLobbyResponse
    users <- Map.keys . dataUsers <$> get
    forM_ users $ \user -> do
        record <- lookup user
        when (isNothing . dataLobby $ record) $
            liftIO . dataReply record $ response

{- =============================================================== -}

registerUser
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => User -> String -> (GameResponse -> IO ()) -> m ()
registerUser user name onReply = do
    users <- Map.keys . dataUsers <$> get
    if elem user users then
        throwError $ (show user) ++ " already registered!"
    else
        insert user (UserData name Nothing onReply)
    sendLobbies user

unregisterUser
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => User -> m ()
unregisterUser user = do
    record <- lookup user
    case dataLobby record of
        Nothing -> return ()
        Just _  -> handleUserAction user LeaveLobby
    delete user

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


handleUserAction
   :: (MonadIO m, MonadState ServerData m, MonadError String m)
   => User -> GameRequest -> m ()

handleUserAction user (MakeMove move) = do
    lobby <- lookupLobby user
    using lobby $ do
        position <- lookupPlayerPosition user
        state <- dataSkatState <$> get
        newState <- liftEither $ play state position move
        modify $ \record -> record {dataSkatState = newState}
    broadcastSkatState lobby

handleUserAction user (SetName name) = do
    record <- lookup user
    insert user record {dataUserName = name}
    case dataLobby record of
        Just lobby -> broadcastSkatState lobby
        Nothing    -> return ()
    broadcastLobbies

handleUserAction user Resign = do
    lobby <- lookupLobby user
    needsRestart <- using lobby $ do
        position <- lookupPlayerPosition user
        adjust position (\record -> record {dataResigned = True})
        checkRestartGame
    when needsRestart $ do
        using lobby $ do
            newGame
            rotatePositions
        broadcastLobbies
    broadcastSkatState lobby

handleUserAction user (JoinLobby uid position) = do
    let lobby = Lobby uid
    addPlayerToLobby user lobby position
    broadcastSkatState lobby
    broadcastLobbies

handleUserAction user LeaveLobby = do
    lobby <- lookupLobby user
    removePlayerFromLobby user
    broadcastSkatState lobby
    broadcastLobbies

handleUserAction user (ChangePosition newPlayerPosition) = do
    lobby <- lookupLobby user
    oldPlayerPosition <- using lobby $ lookupPlayerPosition user
    let newPosition position =
            if      position == newPlayerPosition then oldPlayerPosition
            else if position == oldPlayerPosition then newPlayerPosition
            else                                       position
    using lobby $ changePositions newPosition
    broadcastSkatState lobby
    broadcastLobbies

handleUserAction _ (SpectateLobby _) = do
    throwError "SpectateLobby not implemented!"

handleUserAction _ LeaveSpectate = do
    throwError "LeaveSpectate not implemented!"


joinFreeLobby
    :: (MonadState ServerData m, MonadError String m, MonadIO m)
    => User -> m ()
joinFreeLobby user = do
    lobbies <- Map.keys . dataLobbies <$> get
    avail <- forM lobbies $ flip using freePositions
    let free = List.find (not . null . snd) $ zip lobbies avail
    case free of
        Just (lobby, position:_) -> do
            addPlayerToLobby user lobby position
            broadcastSkatState lobby
            broadcastLobbies
        _ -> throwError "No free lobby found!"
