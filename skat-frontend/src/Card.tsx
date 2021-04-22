import { ICard } from "./State"

export const Card: React.FC<{ card: ICard, onClick?: () => void, player?: string }> = ({ card, onClick, player }) => {
    const { suit, name } = card
    return <div style={{
        width: '4em',
        height: '6em',
        color: 'black',
        background: 'white',
        borderColor: suit === 'Diamonds' || suit === 'Hearts' ? 'red' : 'black',
        borderStyle: 'dashed',
        margin: '.1em',
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'space-between'
    }}>
        <span>{suit}<br />{name}</span>
        {onClick ? <button style={{ height: '2em', margin: '.4em' }} onClick={onClick}>nimm mich</button> : null}
        {player ? <div style={{ color: 'grey' }}>{player}</div> : null}
    </div>
}