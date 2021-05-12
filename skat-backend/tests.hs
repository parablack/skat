{-# LANGUAGE OverloadedStrings #-}


import Definitions
import Skat
import Serializer
import Control.Exception
import Data.List
import Data.Aeson
import Data.Map
import qualified Data.ByteString.Lazy as B


fromRight (Left x)  = error "Got left in FromRight"
fromRight (Right x) = x


turn1 = fromRight $ play debugRamschState Vorhand (Card Ten Hearts)
turn2 = fromRight $ play turn1 Mittelhand (Card King Hearts)
turn3 = fromRight $ play turn2 Geber (Card Eight Hearts)

main = B.putStr $ encode (SkatStateForPlayer Vorhand turn1 Data.Map.empty)

packet1 = "{\"action\":\"showcards\"}"
packet2 = "{\"action\":\"playcard\", \"card\":{\"name\":\"Jack\",\"suit\":\"Hearts\"}}"
res1 = eitherDecode packet1 :: Hopefully GameRequest
res2 = eitherDecode packet2 :: Hopefully GameRequest
