import './ImageCard.css';
import { ICard } from "../State"
import { geileWerteMap } from './SimpleCard';

const cornerImages: { [suit: string]: any } = {
    "Diamonds": <img alt="" src="/images/diamonds.png" style={{ transform: "translate(-50%, -50%) scale(0.04)" }} />,
    "Hearts": <img alt="" src="/images/hearts.png" style={{ transform: "translate(-50%, -50%) scale(0.04)" }} />,
    "Clubs": <img alt="" src="/images/clubs.png" style={{ transform: "translate(-50%, -50%) scale(0.04)" }} />,
    "Spades": <img alt="" src="/images/spades.png" style={{ transform: "translate(-50%, -50%) scale(0.04)" }} />,
}

const numberImages: { [suit: string]: any } = {
    "Diamonds": <img alt="" src="/images/diamonds.png" style={{ transform: "translate(-50%, -50%) scale(0.065)" }} />,
    "Hearts": <img alt="" src="/images/hearts.png" style={{ transform: "translate(-50%, -50%) scale(0.065)" }} />,
    "Clubs": <img alt="" src="/images/clubs.png" style={{ transform: "translate(-50%, -50%) scale(0.065)" }} />,
    "Spades": <img alt="" src="/images/spades.png" style={{ transform: "translate(-50%, -50%) scale(0.065)" }} />,
}

const jackImages: { [suit: string]: any } = {
    "Diamonds": <img alt="" src="/images/diamonds-jack.png" style={{ transform: "translate(-50%, -50%) scale(0.2)" }} />,
    "Hearts": <img alt="" src="/images/hearts-jack.png" style={{ transform: "translate(-50%, -50%) scale(0.2)" }} />,
    "Clubs": <img alt="" src="/images/clubs-jack.png" style={{ transform: "translate(-50%, -50%) scale(0.2)" }} />,
    "Spades": <img alt="" src="/images/spades-jack.png" style={{ transform: "translate(-50%, -50%) scale(0.2)" }} />,
}

const kingImages: { [suit: string]: any } = {
    "Diamonds": <img alt="" src="/images/diamonds-king.png" style={{ transform: "translate(-50%, -46%) scale(0.25)" }} />,
    "Hearts": <img alt="" src="/images/hearts-king.png" style={{ transform: "translate(-50%, -46%) scale(0.25)" }} />,
    "Clubs": <img alt="" src="/images/clubs-king.png" style={{ transform: "translate(-50%, -46%) scale(0.25)" }} />,
    "Spades": <img alt="" src="/images/spades-king.png" style={{ transform: "translate(-50%, -46%) scale(0.25)" }} />,
}

const aceImages: { [suit: string]: any } = {
    "Diamonds": <img alt="" src="/images/diamonds-ace.png" style={{ transform: "translate(-50%, -50%) scale(0.25)" }} />,
    "Hearts": <img alt="" src="/images/hearts-ace.png" style={{ transform: "translate(-50%, -50%) scale(0.25)" }} />,
    "Clubs": <img alt="" src="/images/clubs-ace.png" style={{ transform: "translate(-50%, -50%) scale(0.25)" }} />,
    "Spades": <img alt="" src="/images/spades-ace.png" style={{ transform: "translate(-50%, -50%) scale(0.25)" }} />,
}

const specialImages: { [name: string]: { [suit: string]: any } } = {
    "Jack": jackImages,
    "King": kingImages,
    "Ace": aceImages
}

const Corner: React.FC<{ card: ICard }> = ({ card }) => {
    const { suit, name } = card;
    return <>
        <div className="position-corner">
            <span>{geileWerteMap[name]}</span>
            <span className="position-symbol-corner">
                {cornerImages[suit]}
            </span>
        </div>
    </>
}

const NumbersTop: React.FC<{ card: ICard }> = ({ card }) => {
    const { suit, name } = card;
    const image = numberImages[suit];
    if (name === "Seven" || name === "Eight") {
        return <>
            <span className="position-symbol-0">{image}</span>
            <span className="position-symbol-1">{image}</span>
            <span className="position-symbol-2">{image}</span>
            <span className="position-symbol-3">{image}</span>
            <span className="position-symbol-4">{image}</span>
        </>
    }
    if (name === "Nine") {
        return <>
            <span className="position-symbol-0">{image}</span>
            <span className="position-symbol-1">{image}</span>
            <span className="position-symbol-5">{image}</span>
            <span className="position-symbol-6">{image}</span>
            <span className="position-symbol-7">{image}</span>
        </>
    }
    if (name === "Ten") {
        return <>
            <span className="position-symbol-0">{image}</span>
            <span className="position-symbol-1">{image}</span>
            <span className="position-symbol-5">{image}</span>
            <span className="position-symbol-7">{image}</span>
            <span className="position-symbol-8">{image}</span>
        </>
    }
    return <></>
}

const NumbersBottom: React.FC<{ card: ICard }> = ({ card }) => {
    const { suit, name } = card;
    const image = numberImages[suit];
    if (name === "Seven") {
        return <>
            <span className="position-symbol-0">{image}</span>
            <span className="position-symbol-1">{image}</span>
        </>
    }
    if (name === "Eight") {
        return <>
            <span className="position-symbol-0">{image}</span>
            <span className="position-symbol-1">{image}</span>
            <span className="position-symbol-3">{image}</span>
        </>
    }
    if (name === "Nine") {
        return <>
            <span className="position-symbol-0">{image}</span>
            <span className="position-symbol-1">{image}</span>
            <span className="position-symbol-5">{image}</span>
            <span className="position-symbol-7">{image}</span>
        </>
    }
    if (name === "Ten") {
        return <>
            <span className="position-symbol-0">{image}</span>
            <span className="position-symbol-1">{image}</span>
            <span className="position-symbol-5">{image}</span>
            <span className="position-symbol-7">{image}</span>
            <span className="position-symbol-8">{image}</span>
        </>
    }
    return <></>
}

const SpecialCenter: React.FC<{ card: ICard }> = ({ card }) => {
    const { suit, name } = card;
    if (["Jack", "King", "Ace"].includes(name)) { // TODO queen
        return <>
            <div className="position-center">
                {specialImages[name][suit]}
            </div>
        </>
    }
    return <></>
}

export const Card: React.FC<{
    card: ICard,
    onClick?: () => void,
    player?: string
}> = ({ card, onClick, player }) => {

    const { suit } = card
    return <>
        <div
            className="card"
            onClick={onClick}
            style={{
                color: (suit === "Diamonds" || suit === "Hearts") ? "red" : "black" // geileFarbenMap[suit]
            }}
        >
            <div className="position-top">
                <Corner card={card} />
                <NumbersTop card={card} />
            </div>
            <div className="position-bottom">
                <Corner card={card} />
                <NumbersBottom card={card} />
            </div>
            <SpecialCenter card={card} />
        </div>
    </>
}
