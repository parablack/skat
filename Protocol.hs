import Definitions
import Skat
import Serializer


{-

    Server -> Client:
        Vor-Vorbereitung:
            Deine ID ist X
            Der Name von Spieler Y ist Z
        Vorbereitung:
            Spieler Y hat diese Karten
            Spieler X ist am Zug mit reizen
            Spieler X hat Z gereizt
            Spieler X hat das Reizen gewonnen
            Hier ist der Skat!

        Spielstart:
            Diese Variante + Farbe wird gespielt
            Y ist am Zug
            Y hat diese Karte gespielt
            Dieser Stich geht an Y
            Das Spiel ist zu Ende! {Ergebnis}

    Client -> Server:
        Vor-Vorbereitung:
            Mein Name ist X
        Vorbereitung:
            Ich reize Z
            Ich möchte den Skat sehen
            Ich will diese Karten in den Skat legen
            Ich spiele Farbe Z + Variante Y
        Spiel:
            Ich spiele die Karte Z

-}


