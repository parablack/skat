{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Serializer where


import Control.Exception
import Data.List
import Definitions
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.Map
import qualified Data.ByteString.Lazy as B
import Skat

instance ToJSON Card where
    toJSON (Card name suit)= object ["suit" .= suit, "name" .= name]
instance ToJSON GameMode where
    toJSON = Data.Aeson.String . pack . nicesShow



instance ToJSON Player where
    toJSON Player{playerPosition = pos, playerCards = cards, wonCards = won} = object ["position" .= pos, "cards" .= sort cards, "woncards" .= won]

instance ToJSON SkatState where
    toJSON state@ReizPhase{players=players} = object ["phase" .= pack "reiz", "players" .= players]
    toJSON state@RunningPhase{} =
        object [
            "phase" .= pack "running",
            "players" .= players state,
            "gamemode" .= gameMode state,
            "currentstich" .= currentStich state,
            "turn" .= turn state
        ]

-- personalizedSkatState :: SkatState -> PlayerPosition -> [Data.Aeson.Types.Internal.Pair]
personalizedSkatState state@ReizPhase{} player = [
                "phase" .= pack "reiz"
            ]
personalizedSkatState state@RunningPhase{} player = [
                "phase" .= pack "running",
                "gamemode" .= gameMode state,
                "currentStich" .= Data.List.reverse (currentStich state),
                "lastStich" .= case playedStiche state of
                    [] -> []
                    (x:xs) -> Data.List.reverse x,
                "yourTurn" .=  (player == turn state),
                "turn" .= turn state
            ]
personalizedSkatState state@GameFinishedState{} player = [
                "phase" .= pack "finished",
                "currentStich" .= lastStich state,
                "yourTurn" .= False,
                "scores" .= Data.Map.mapKeys show (scores state),
                "winner" .= winner state
            ]

instance ToJSON SkatStateForPlayer where
    toJSON (SkatStateForPlayer player state names resigning) =
        object $ [
                "you" .= playerFromPos state player,
                "names" .= names,
                "resign" .= resigning
                ] ++ additionalProperties
        where additionalProperties = personalizedSkatState state player



instance FromJSON Card where
    parseJSON (Object card) =
        Card <$> (card .: "name") <*> card .: "suit"
    parseJSON _ = parseFail "Card must be an object."

instance FromJSON ReceivePacket where
    parseJSON (Object obj) = do
            action <- obj .: "action" :: Parser Text
            case action of
                "showcards" -> return ShowCards
                "playcard" -> PlayCard <$> (obj .: "card" :: Parser Card)
                "setname"  -> SetName  <$> (obj .: "name" :: Parser String)
                "playvariant"  -> PlayVariant . gameModeFromString <$> (obj .: "variant" :: Parser String)
                "discardskat" -> DiscardSkat <$>
                                 obj .: "card1" <*>
                                 obj .: "card2"
                "resign" -> return Resign
                _ -> parseFail "Action unspecified."

    parseJSON _ = parseFail "Got no object."