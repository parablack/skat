{-# LANGUAGE OverloadedStrings #-}

module Serializer where


import Control.Exception
import Data.List
import Definitions
import Data.Aeson
import Data.Text

instance ToJSON PlayerPosition where
    toJSON = Data.Aeson.String . pack . show
instance ToJSON Suit where
    toJSON = Data.Aeson.String . pack . show
instance ToJSON Name where
    toJSON = Data.Aeson.String . pack . show
instance ToJSON Card where
    toJSON (Card name suit)= object ["suit" .= suit, "name" .= name]
instance ToJSON GameMode where
    toJSON = Data.Aeson.String . pack . nicesShow



instance ToJSON Player where
    toJSON Player{playerPosition = pos, playerCards = cards, wonCards = won} = object ["position" .= pos, "cards" .= cards, "woncards" .= won]

instance ToJSON SkatState where
    toJSON state@ReizPhase{players=players} = object ["phase" .= pack "reiz", "players" .= players]
    toJSON state@RunningPhase{players=players,gameMode=gameMode, currentStich=stich, turn=turn} = object ["phase" .= pack "running", "players" .= players, "gamemode" .= gameMode, "currentstich" .= stich, "turn" .= turn]
