import logo from './logo.svg';
import './App.css';
import { useState } from 'react';

interface ICard { color: string, value: string }

const Card: React.FC<{ card: ICard, onClick?: () => void, player?: string }> = ({ card, onClick, player }) => {
  const { color, value } = card
  return <div style={{
    width: '4em',
    height: '6em',
    color: 'black',
    background: 'white',
    borderColor: color === 'Herz' || color === 'Karo' ? 'red' : 'black',
    borderStyle: 'dashed',
    margin: '.1em',
    display: 'flex',
    flexDirection: 'column',
    justifyContent: 'space-between'
  }}>
    <span>{color}<br />{value}</span>
    {onClick ? <button style={{ height: '2em', margin: '.4em' }} onClick={onClick}>nimm mich</button> : null}
    {player ? <div style={{ color: 'grey' }}>{player}</div> : null}
  </div>
}

const Hand: React.FC<{ cards: ICard[] }> = ({ cards }) => {
  return <div style={{ display: 'flex', fontSize: '.8em' }}>
    {cards.map((card, index) => {
      const degreeSpread = 20
      const magicFactor = 15   // FIXME >:(
      let f = (index + .5) / cards.length
      let rot = degreeSpread * (f - .5)
      let rrot = rot * Math.PI / 180
      return <span style={{
        transform: `translate(0em, ${Math.abs(Math.sin(rrot)) * magicFactor}em) rotate(${rot}deg)`,
        transformOrigin: '50% 50%',
      }}><Card card={card} onClick={() => alert("aluurm " + JSON.stringify(card))}></Card></span>
    })}
  </div>
}


const TableStack: React.FC<{ cards: ICard[] }> = ({ cards }) => {
  return <div style={{ fontSize: '.8em', background: 'grey', minWidth: '40vmin', minHeight: '40vmin' }}>
    {cards.length ? (
      <span style={{ display: 'flex', flexDirection: 'row', width: '100%', height: '100%', justifyContent: 'center' }}>
        {cards.map((card, index) => {
          return <span style={{}}><Card card={card} player={["Simon", "pinguly", "mflo"][index % 3]}></Card></span>
        })}
      </span>
    ) : (
      <img src={logo} className="App-logo" alt="logo" />
    )}
  </div>
}

function App() {
  const [hand, setHand] = useState([
    { color: "Herz", value: "Ober" },
    { color: "Pik", value: "Acht" },
    { color: "Kreuz", value: "Unter" },
    { color: "Karo", value: "Ass" },
    { color: "Karo", value: "Bins" },
    { color: "Herz", value: "Bwei" },
    { color: "Kreuz", value: "Brei" },
    { color: "Pik", value: "Bier" },
    { color: "Karo", value: "Bünf" },
    { color: "Pik", value: "Bechs" },
  ])

  const [stack, setStack] = useState([
    { color: 'Kreuz', value: 'Bier' }
  ])

  return (
    <div className="App">
      <section style={{
        backgroundColor: '#282c34',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        // justifyContent: 'center',
        justifyContent: 'space-between',
        fontSize: 'calc(10px + 2vmin)',
        color: 'white',
        height: '100vh',
      }}>
        <header>
          Wilkommen im Ramschsimulator 3000
          <br />
          <small>Heute spielen Sie: Ramsch</small>
          <br />
          <button onClick={() => setStack([
          ])}>0</button>
          <button onClick={() => setStack([
            { color: "Karo", value: "Ass" },
          ])}>1</button>
          <button onClick={() => setStack([
            { color: "Karo", value: "Ass" },
            { color: "Karo", value: "Bins" },
          ])}>2</button>
          <button onClick={() => setStack([
            { color: "Karo", value: "Ass" },
            { color: "Karo", value: "Bins" },
            { color: "Herz", value: "Bwei" },
          ])}>3</button>
        </header>
        <TableStack cards={stack} />
        <p>
          Aluurm! Du bist dran! <button onClick={() => {
            function shuffle<T>(a: T[]) {
              for (let i = a.length - 1; i > 0; i--) {
                const j = Math.floor(Math.random() * (i + 1));
                [a[i], a[j]] = [a[j], a[i]];
              }
              return a;
            }
            setHand(shuffle([...hand]))
          }}>Aufgeben</button>
        </p>
        <span style={{ margin: '4em' }}>
          <Hand cards={hand} />
        </span>
      </section>
    </div>
  );
}

export default App;
