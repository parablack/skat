{-# LANGUAGE OverloadedStrings #-}


import Definitions
import Skat
import Serializer
import Control.Exception
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy as B

slice start end = take (end - start + 1) . drop start

debugdeck = [Card name suit | name <- names, suit <- suits]
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
debugRamschState = RunningPhase {
    players = [Player Geber (slice 0 9 debugdeck) [],
            Player Vorhand (slice 10 19 debugdeck) [],
            Player Mittelhand (slice 20 29 debugdeck) []
            ],
    singlePlayer = Nothing,
    gameMode = mRamsch,
    currentStich = [],
    turn = Vorhand
}

fromRight (Left x)  = error "Got left in FromRight"
fromRight (Right x) = x


turn1 = fromRight $ play debugRamschState Vorhand (Card Ten Hearts)
turn2 = fromRight $ play turn1 Mittelhand (Card King Hearts)
turn3 = fromRight $ play turn2 Geber (Card Eight Hearts)

main = B.putStr $ encode turn3

packet1 = "{\"action\":\"showcards\"}"
packet2 = "{\"action\":\"playcard\", \"card\":{\"name\":\"Jack\",\"suit\":\"Hearts\"}}"
res1 = eitherDecode packet1 :: Hopefully ReceivePacket
res2 = eitherDecode packet2 :: Hopefully ReceivePacket