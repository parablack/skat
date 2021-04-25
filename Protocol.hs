import Definitions
import Skat
import Serializer


{-

    phase: reiz
    - yourTurn :: Bool,
    - reizAnsagerTurn :: Bool,
    - reizCurrentBid :: Int
    - turn

    Wenn reizAnsagerTurn:
        Frage nach Zahl (> reizCurrentBid) oder "Weg"
        {"action": "reizbid", "bid": 18}
        {"action": "reizbid", "bid": 0} für Weg
    Wenn reizAnsagerTurn == False:
        Frage nach "Ja / Nein"
        {"action": "reizanswer", "value":: bool}


-}


