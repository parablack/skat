{-# LANGUAGE OverloadedStrings #-}

module Serializer where

import Skat.Definitions
    ( PlayerPosition(..)
    , Card(..)
    , GameMode(..)
    , SkatScoringInformation(..)
    , ScoringResult(..)
    , SkatGewinnstufe(..)
    , SkatMove(..)
    , Reizwert(..)
    )
import Skat.GameModes (gameModeFromString)

import GameServer.Protocol
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.Map as Map

instance ToJSON Card where
    toJSON (Card name suit) =
        object [ "suit" .= suit
               , "name" .= name
               ]

instance ToJSON CensoredCard where
    toJSON (NotCensored card) = toJSON card

    toJSON Censored =
        object [ "suit" .= pack "?"
               , "name" .= pack "?"
               ]

instance ToJSON GameMode where
    toJSON gm =
        object [ "kind"  .= fst meta
               , "color" .= snd meta
               ]
            where meta = nicesShow gm

instance ToJSON SkatScoringInformation where
    toJSON info =
        object [ "hand"     .= isHand info
               , "angesagt" .= angesagteStufe info
               ]

instance ToJSON ScoringResult where
   toJSON result =
       object [ "position"      .= scoringPosition result
              , "hasWon"        .= scoringHasWon result
              , "gameValue"     .= scoringGameValue result
              , "HasUeberreizt" .= scoringHasUeberreizt result
              ]

instance ToJSON PublicInfo where
    toJSON info =
        object [ "turn"        .= infoTurn info
               , "cards"       .= Map.mapKeys show (infoCards info)
               , "names"       .= Map.mapKeys show (infoNames info)
               , "numResigned" .= infoNumResigned info
               ]

instance ToJSON PrivateInfo where
   toJSON info =
       object [ "yourPosition" .= infoYourPosition info
              , "yourTurn"     .= infoYourTurn info
              , "yourCards"    .= infoYourCards info
              , "wonCards"     .= infoWonCards info
              , "showingCards" .= infoShowingCards info
              , "resigned"     .= infoResigned info
              ]

instance ToJSON PickingSubPhase where
    toJSON = toJSON . show

instance ToJSON PhaseInfo where
    toJSON phase@ReizPhaseInfo{} =
        object [ "phase"         .= pack "ReizPhase"
               , "isAnsagerTurn" .= infoIsAnsagerTurn phase
               , "bid"           .= infoBid phase
               ]

    toJSON phase@PickingPhaseInfo{} =
        object [ "phase"          .= pack "PickingPhase"
               , "subPhase"       .= infoSubPhase phase
               , "pickingPlayer"  .= infoPickingPlayer phase
               , "cardsToDiscard" .= infoCardsToDiscard phase
               , "isPlayingHand"  .= infoIsPlayingHand phase
               ]

    toJSON phase@RunningPhaseInfo{} =
        object [ "phase"        .= pack "RunningPhase"
               , "gameMode"     .= infoGameMode phase
               , "scoring"      .= infoScoring phase
               , "currentStich" .= infoCurrentStich phase
               , "lastStich"    .= infoLastStich phase
               , "singlePlayer" .= infoSinglePlayer phase
               ]

    toJSON phase@FinishedPhaseInfo{} =
        object [ "phase"         .= pack "FinishedPhase"
               , "lastStich"     .= infoLastStich phase
               , "scores"        .= Map.mapKeys show (infoScores phase)
               , "scoringResult" .= infoScoringResult phase
               ]

instance ToJSON LobbyInformation where
    toJSON lobby@LobbyInformation{} =
        object [ "id"    .= infoLobbyId lobby
               , "name"  .= infoLobbyName lobby
               , "names" .= Map.mapKeys show (infoLobbyPositions lobby)
               ]

instance ToJSON GameResponse where
    toJSON (StatePlayerResponse phaseInfo publicInfo privateInfo) =
        object [ "type"    .= pack "PlayerState"
               , "phase"   .= phaseInfo
               , "public"  .= publicInfo
               , "private" .= privateInfo
               ]

    toJSON (StateSpectatorResponse phaseInfo publicInfo) =
        object [ "type"   .= pack "SpectatorState"
               , "phase"  .= phaseInfo
               , "public" .= publicInfo
               ]

    toJSON (LobbyResponse lobbies) =
        object [ "type"    .= pack "LobbyState"
               , "lobbies" .= lobbies
               ]


instance FromJSON Card where
    parseJSON (Object card) =
        Card <$> (card .: "name") <*> card .: "suit"
    parseJSON _ = parseFail "Card must be an object."

instance FromJSON GameMode where
    parseJSON (String card) = do
        case gameModeFromString (unpack card) of
            Right var -> return var
            Left err -> parseFail err
    parseJSON _ = parseFail "PlayVariant must be a string."


instance FromJSON GameRequest where
    parseJSON (Object obj) = do
        action <- obj .: "action" :: Parser Text
        case action of
            "showcards"   -> return $ MakeMove ShowCards
            "playcard"    -> MakeMove . PlayCard <$> (obj .: "card" :: Parser Card)
            "setname"     -> SetName  <$> (obj .: "name" :: Parser String)
            "playvariant" -> MakeMove <$> (PlayVariant <$> (obj .: "variant" :: Parser GameMode) <*> (obj .: "angesagt" :: Parser SkatGewinnstufe))
            "discardskat" -> MakeMove <$> (DiscardSkat <$> (obj .: "card1") <*>  (obj .: "card2"))
            "resign"      -> return Resign
            "reizbid"     -> (MakeMove . ReizBid . Reizwert) <$> (obj .: "reizbid" :: Parser Int)
            "reizweg"     -> return . MakeMove $ ReizBid Weg
            "reizanswer"  -> MakeMove . ReizAnswer <$> (obj .: "value" :: Parser Bool)
            "playhand"    -> MakeMove . PlayHand <$> (obj .: "hand" :: Parser Bool)
            "join"        -> JoinLobby <$> (obj .: "id" :: Parser Int) <*> (obj .: "position" :: Parser PlayerPosition)
            "leave"       -> return LeaveLobby
            "changepos"   -> ChangePosition <$> (obj .: "position" :: Parser PlayerPosition)
            "joinspec"    -> SpectateLobby <$> (obj .: "id" :: Parser Int)
            "leavespec"   -> return LeaveSpectate
            _             -> parseFail "Action unspecified."

    parseJSON _ = parseFail "Got no object."
