import logo from './logo.svg';
import './App.css';
import React from 'react';
import { DEBUG_STATE, ICard } from './State';
import { Card } from './Card';
import { Hand } from './Hand';


const TableStack: React.FC<{ cards: ICard[] }> = ({ cards }) => {
  return <div style={{ fontSize: '.8em', background: 'grey', minWidth: '40vmin', minHeight: '40vmin' }}>
    {cards.length ? (
      <span style={{ display: 'flex', flexDirection: 'row', width: '100%', height: '100%', justifyContent: 'center' }}>
        {cards.map((card, index) => {
          return <span style={{}}>
            <Card card={card} player={["Simon", "pinguly", "mflo"][index % 3]}></Card>
          </span>
        })}
      </span>
    ) : (
      <img src={logo} className="App-logo" alt="logo" />
    )}
  </div>
}

function App() {
  let state = DEBUG_STATE

  return (
    <div className="App">
      <section style={{
        backgroundColor: '#282c34',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'space-between',
        fontSize: 'calc(10px + 2vmin)',
        color: 'white',
        height: '100vh',
      }}>
        <header>
          Wilkommen im Ramschsimulator 3000
          <br />
          <small>Heute spielen Sie: {state.gamemode}</small>
        </header>
        <TableStack cards={state.currentStich.map(([card, _player]) => card)} />
        <p>
          Aluurm! Du bist dran!
        </p>
        <span style={{ margin: '4em' }}>
          <Hand cards={state.you.cards} />
        </span>
      </section>
    </div>
  );
}

export default App;
