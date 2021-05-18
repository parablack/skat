import './ImageCard.css';
import { Card as ICard } from "../State"
import { geileWerteMap } from './SimpleCard';

const image = function (name: string, width: number, dx?: number, dy?: number): any {
  dx = dx ? dx - 50 : -50;
  dy = dy ? dy - 50 : -50;
  return <img
    alt=""
    src={`/images/${name}.png`}
    style={{
      transform: `translate(${dx}%, ${dy}%)`,
      width: `${width}em`
    }}
  />
}

const cornerImages: { [suit: string]: any } = {
  "Diamonds": image("diamonds", 0.9),
  "Hearts": image("hearts", 0.9),
  "Clubs": image("clubs", 0.9),
  "Spades": image("spades", 0.9),
}

const numberImages: { [suit: string]: any } = {
  "Diamonds": image("diamonds", 1.5),
  "Hearts": image("hearts", 1.5),
  "Clubs": image("clubs", 1.5),
  "Spades": image("spades", 1.5),
}

const jackImages: { [suit: string]: any } = {
  "Diamonds": image("diamonds-jack", 4.5),
  "Hearts": image("hearts-jack", 4.5),
  "Clubs": image("clubs-jack", 4.5),
  "Spades": image("spades-jack", 4.5),
}

const queenImages: { [suit: string]: any } = {
  "Diamonds": image("diamonds-queen", 5, 0, -10),
  "Hearts": image("hearts-queen", 5, 0, -10),
  "Clubs": image("clubs-queen", 5, 0, -10),
  "Spades": image("spades-queen", 5, 0, -10),
}

const kingImages: { [suit: string]: any } = {
  "Diamonds": image("diamonds-king", 5, 0, 15),
  "Hearts": image("hearts-king", 5, 0, 15),
  "Clubs": image("clubs-king", 5, 0, 15),
  "Spades": image("spades-king", 5, 0, 15),
}

const aceImages: { [suit: string]: any } = {
  "Diamonds": image("diamonds-ace", 5),
  "Hearts": image("hearts-ace", 5),
  "Clubs": image("clubs-ace", 5),
  "Spades": image("spades-ace", 5),
}

const specialImages: { [name: string]: { [suit: string]: any } } = {
  "Jack": jackImages,
  "Queen": queenImages,
  "King": kingImages,
  "Ace": aceImages
}

const colorMap : { [suit: string]: string } = {
  "Diamonds": "card-color-diamonds",
  "Hearts":   "card-color-hearts",
  "Clubs":    "card-color-clubs",
  "Spades":   "card-color-spades"
}

const Corner: React.FC<{ card: ICard }> = ({ card }) => {
  const { suit, name } = card;
  return <>
    <div className="position-corner" style={{
      letterSpacing: (name === "Ten") ? "-0.1em" : "0em"
    }}>
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
  if (["Jack", "King", "Queen", "Ace"].includes(name)) {
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
}> = ({ card, onClick }) => {
  const { suit } = card
  return <>
    <div
      className={"card " + colorMap[suit]}
      onClick={onClick}
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
