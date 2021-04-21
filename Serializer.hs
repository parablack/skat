{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Serializer where


import Control.Exception
import Data.List
import Definitions
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.ByteString.Lazy as B
import Skat

instance ToJSON Card where
    toJSON (Card name suit)= object ["suit" .= suit, "name" .= name]
instance ToJSON GameMode where
    toJSON = Data.Aeson.String . pack . nicesShow


instance ToJSON Player where
    toJSON Player{playerPosition = pos, playerCards = cards, wonCards = won} = object ["position" .= pos, "cards" .= cards, "woncards" .= won]

instance ToJSON SkatState where
    toJSON state@ReizPhase{players=players} = object ["phase" .= pack "reiz", "players" .= players]
    toJSON state@RunningPhase{players=players,gameMode=gameMode, currentStich=stich, turn=turn} = object ["phase" .= pack "running", "players" .= players, "gamemode" .= gameMode, "currentstich" .= stich, "turn" .= turn]


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
                _ -> parseFail "Action unspecified."

    parseJSON _ = parseFail "Got no object."