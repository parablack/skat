import { ICard } from "./State"

const geileMap:{[name:string]: string} = {
    "Diamonds": "♦",
    "Hearts": "♥",
    "Spades": "♠",
    "Clubs": "♣"
}
const geileFarbenMap:{[name:string]: string} = {
    "Diamonds": "orange",
    "Hearts": "red",
    "Spades": "green",
    "Clubs": "black"
}
const geileWerteMap:{[name:string]: string} = {
    "Ten": "10",
    "Seven": "7",
    "Eight": "8",
    "Nine": "9",
    "Jack": "J",
    "Queen": "Q",
    "King": "K",
    "Ace": "A"
}


export const Card: React.FC<{ card: ICard, onClick?: () => void, player?: string }> = ({ card, onClick, player }) => {
    const { suit, name } = card
    return <div style={{
        width: '6rem',
        height: '8rem',
        fontSize: '2em',
        background: 'white',
        borderColor: geileFarbenMap[suit],
        color: geileFarbenMap[suit],
        borderStyle: 'dashed',
        margin: '.1em',
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'space-between'
    }} onClick={onClick}>
        <span style={{
            textAlign: 'left'
        }}>{geileWerteMap[name]}{geileMap[suit]}</span>
    {/*   {onClick ? <button style={{ height: '2em', margin: '.4em' }} onClick={onClick}>nimm mich</button> : null} */}
        {player ? <div style={{ color: 'grey', fontSize: '0.5em' }}>{player}</div> : null}
    </div>
}