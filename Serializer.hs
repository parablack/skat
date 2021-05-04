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
    toJSON (Card name suit) = object ["suit" .= suit, "name" .= name]

instance ToJSON GameMode where
    toJSON gm = object ["kind" .= fst meta,
                     "color" .= snd meta ]
                where meta = nicesShow gm

instance ToJSON SkatScoringInformation where
    toJSON info = object ["hand" .= isHand info,
                     "angesagt" .= angesagteStufe info ]



instance ToJSON Player where
    toJSON Player{playerPosition = pos, playerCards = cards, wonCards = won} = object ["position" .= pos, "cards" .= sort cards, "woncards" .= won]

-- personalizedSkatState :: SkatState -> PlayerPosition -> [Data.Aeson.Types.Internal.Pair]
personalizedSkatState state@ReizPhase{} player = [
                "phase" .= pack "reizen",
                "yourTurn" .= (reizTurn state == Just player),
                "turn" .= (case reizTurn state of
                    Nothing -> error "Unreachable state: Nobody's turn"
                    Just x -> x),
                "reizAnsagerTurn" .= reizAnsagerTurn state,
                "reizCurrentBid" .= reizCurrentBid state
            ]
personalizedSkatState state@SkatPickingPhase{} player = [
                "phase" .= pack "skatpicking",
                "yourTurn" .= (singlePlayer state == Just player),
                "turn" .= (case singlePlayer state of
                    Nothing -> error "Unreachable state: Nobody's turn (no single player in skat picking)"
                    Just x -> x),
                "cardsToDiscard" .=
                    (case singlePlayer state of
                    Nothing -> error "Unreachable state: Nobody's turn (no single player in skat picking)"
                    Just x -> (Data.List.length . playerCards) (playerFromPos state player) - 10
                    )
            ]
personalizedSkatState state@GamePickingPhase{} player = [
                "phase" .= pack "gamepicking",
                "yourTurn" .= (singlePlayer state == Just player),
                "turn" .= (case singlePlayer state of
                    Nothing -> error "Unreachable state: Nobody's turn (no single player in skat picking)"
                    Just x -> x)
            ]
personalizedSkatState state@HandPickingPhase{} player = [
                "phase" .= pack "handpicking",
                "yourTurn" .= (singlePlayer state == Just player),
                "turn" .= (case singlePlayer state of
                    Nothing -> error "Unreachable state: Nobody's turn (no single player in hand picking)"
                    Just x -> x)
            ]
personalizedSkatState state@RunningPhase{} player = [
                "phase" .= pack "running",
                "gamemode" .= gameMode state,
                "scoring" .= skatScoringInformation state,
                "currentStich" .= Data.List.reverse (currentStich state),
                "lastStich" .= case playedStiche state of
                    [] -> []
                    (x:xs) -> Data.List.reverse x,
                "yourTurn" .=  (player == turn state),
                "singlePlayer" .= (case singlePlayer state of
                    Nothing -> "nobody"
                    Just x -> toJSON x),
                "turn" .= turn state
            ]
personalizedSkatState state@GameFinishedState{} player = [
                "phase" .= pack "finished",
                "currentStich" .= Data.List.reverse (lastStich state),
                "yourTurn" .= False,
                "scores" .= Data.Map.mapKeys show (scores state),
                "result" .= result state
            ]

data CensoredCard = Censored | NotCensored Card

instance ToJSON CensoredCard where
    toJSON Censored           = object ["suit" .= ("?" :: Text), "name" .= ("?" :: Text)]
    toJSON (NotCensored card) = toJSON card

censoredCards state showingCards =
    object $ [
        "Geber"      .= censor Geber,
        "Vorhand"    .= censor Vorhand,
        "Mittelhand" .= censor Mittelhand
    ]
  where
      censor position =
          let player = playerFromPos state position
              cards = sort $ playerCards player
          in Data.List.map (\card ->
                if position `elem` showingCards
                    then NotCensored card
                    else Censored
            ) cards

instance ToJSON SkatStateForPlayer where
    toJSON (SkatStateForPlayer player state names resigning showingCards) =
        object $ [
                "you" .= playerFromPos state player,
                "names" .= names,
                "resign" .= resigning,
                "cards" .= censoredCards state showingCards
                ]
                ++ personalizedSkatState state player

instance ToJSON PlayerResponse where

    toJSON (StateResponse state@SkatStateForPlayer{}) = toJSON state
    toJSON (LobbyResponse lobbies) =
        object $ [
            --"phase" .= "lobby",
            --"lobbies" .= []
        ]
        {--
        id: num,
        name: string,
        names: pos -> spieler--}


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


instance FromJSON ReceivePacket where
    parseJSON (Object obj) = do
        action <- obj .: "action" :: Parser Text
        case action of
            "showcards"   -> return ShowCards
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
            _             -> parseFail "Action unspecified."

    parseJSON _ = parseFail "Got no object."
