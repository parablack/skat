{-# LANGUAGE OverloadedStrings #-}

module Serializer where

import Data.List
import Skat.Definitions hiding (SkatState(..), Player(..), reizTurn)
import GameServer.Protocol
import GameServer.Definitions hiding (Player)
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.Map as Map
import Skat.Skat

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
        object [ "turn"           .= pubTurn info
               , "cards"          .= (Map.mapKeys show $ pubCards info)
               , "names"          .= (Map.mapKeys show $ pubNames info)
               , "pubNumResigned" .= pubNumResigned info
               ]

instance ToJSON PrivateInfo where
   toJSON info =
       object [ "yourPosition" .= yourPosition info
              , "yourTurn"     .= yourTurn info
              , "yourCards"    .= yourCards info
              , "wonCards"     .= wonCards info
              , "resigned"     .= resigned info
              ]

instance ToJSON PickingSubPhase where
    toJSON = toJSON . show

instance ToJSON PhaseInfo where
    toJSON phase@ReizPhaseInfo{} =
        object [ "phase"    .= pack "ReizPhase"
               , "reizTurn" .= reizTurn phase
               , "reizBid"  .= reizBid phase
               ]

    toJSON phase@PickingPhaseInfo{} =
        object [ "phase"          .= pack "PickingPhase"
               , "subPhase"       .= subPhase phase
               , "pickingPlayer"  .= pickingPlayer phase
               , "cardsToDiscard" .= cardsToDiscard phase
               , "isPlayingHand"  .= isPlayingHand phase
               ]

    toJSON phase@RunningPhaseInfo{} =
        object [ "phase"        .= pack "RunningPhase"
               , "gameMode"     .= gameMode phase
               , "scoring"      .= scoring phase
               , "currentStich" .= currentStich phase
               , "lastStich"    .= lastStich phase
               , "singlePlayer" .= singlePlayer phase
               ]

    toJSON phase@FinishedPhaseInfo{} =
        object [ "phase"         .= pack "FinishedPhase"
               , "lastStich"     .= lastStich phase
               , "scores"        .= (Map.mapKeys show $ scores phase)
               , "scoringResult" .= scoringResult phase
               ]

instance ToJSON LobbyInformation where
    toJSON LobbyInformation{lobbyId=num, lobbyName=name, lobbyPositions=pos} =
        object [ "id"    .= num
               , "name"  .= name
               , "names" .= Map.mapKeys show pos
               ]

instance ToJSON GameResponse where
    toJSON (StatePlayerResponse phaseInfo publicInfo privateInfo) =
        object [ "type"    .= pack "playerState"
               , "phase"   .= phaseInfo
               , "public"  .= publicInfo
               , "private" .= privateInfo
               ]

    toJSON (StateSpectatorResponse phaseInfo publicInfo) =
        object [ "type"   .= pack "spectatorState"
               , "phase"  .= phaseInfo
               , "public" .= publicInfo
               ]

    toJSON (LobbyResponse lobbies) =
        object [ "type"    .= pack "lobbyState"
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
            _             -> parseFail "Action unspecified."

    parseJSON _ = parseFail "Got no object."
