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
  return <div style={{ display: 'flex', fontSize: '.8em', width: 'var(--cards-width)' }}>
    {
    cards.map((card, index) => {
      let ratio = 84.9667 / 122.567   // ratio cardWidth/cardHeight (including colored border)
      let width = 6;                  // with of the arc (unit: card widths)
      let theta = 27 * Math.PI / 180; // inner angle of the arc (unit: rad)

      let a = (cards.length - 1 - width) / 2; // leftmost x position of arc
      let b = (cards.length - 1 + width) / 2; // rightmost x position of arc

      // magic math follows
      let h = 1e-8;
      let n = 500;

      function f(t: number) {
          let w1 = Math.sin(theta)
          let w2 = Math.cos(theta)
          let z1 = Math.cos(t * Math.PI)
          let z2 = Math.sin(t * Math.PI)
          let x = ((a + b) * (w2 * z2 + 1) + (a - b) * z1) / (2 * w2 * z2 + 2)
          let y = (b - a) * w1 * z2 / (2 * w2 * z2 + 2)
          return [x, y]
      }

      function df(t: number) {
          let [x, y] = f(t)
          let [x2, y2] = f(t + h);
          let [dx, dy] = [(x2 - x) / h, (y2 - y) / h];
          return [dx, dy];
      }

      function arclengthParam(t: number) {
          let res = 0;
          let cache = []; // TODO compute once outside loop
          cache.length = n;
          for (let i = 0; i < n; i++) {
              let [ax, ay] = df(i / n);
              let a = Math.sqrt(ax*ax + ay*ay);
              let [bx, by] = df((i + 1) / n);
              let b = Math.sqrt(bx*bx + by*by);
              res += 0.5 * (a + b) / n;
              cache[i] = res;
          }
          for (let i = 0; i < n; i++) {
              if (cache[i] > t * res) {
                  return (i) / n;
              }
          }
          return 1;
      }

      let t = index / (cards.length - 1); // position on arc between 0 and 1
      let u = arclengthParam(t);
      let [x, y] = f(u);
      let [dx, dy] = df(u);
      let r = -Math.atan2(dy, dx);

      return <span style={{
        transform: `translate(${(x-index)*100}%, ${-ratio*y*100}%) rotate(${r}rad)`,
        transformOrigin: 'center center',
        width: ""
    }}><Card card={card} onClick={() => alert("aluurm " +  JSON.stringify(card))}></Card></span>
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
