import logo from './logo.svg';
import './App.css';
import React, { useEffect, useState } from 'react';
import { DEBUG_STATE, ICard } from './State';
import { Card } from './Card';
import { Hand } from './Hand';


const TableStack: React.FC<{ cards: [ICard, string][] }> = ({ cards }) => {
  return <div style={{ fontSize: '.8em', background: 'grey', minWidth: '40vmin', minHeight: '40vmin' }}>
    {cards.length ? (
      <span style={{ display: 'flex', flexDirection: 'row', width: '100%', height: '100%', justifyContent: 'center' }}>
        {cards.map(([card, name], index) => {
          return <span style={{}} key={index}>
            <Card card={card} player={name}></Card>
          </span>
        })}
      </span>
    ) : (
      <img src={logo} className="App-logo" alt="logo" />
    )}
  </div>
}

export const App: React.FC<{ ws: WebSocket }> = ({ ws }) => {
  const [state, setState] = useState(DEBUG_STATE) /* TODO: remove DEBUG_STATE */
  // const [nickname, setNickname] = useState(undefined)

  let resolveNickname = (pos:string) => state.names[pos] || pos

  useEffect(() => {

    ws.onmessage = e => {
      console.log(e.data)
      let newState = JSON.parse(e.data)
      if (newState.error) {
        alert(newState.error)
      } else {
        setState(newState)
      }
    }

    return () => ws.close()
  }, [ws])

  let displayStich = state.currentStich.length === 0 ? state.lastStich : state.currentStich;

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
        {
        }
        <TableStack cards={displayStich.map(([card, player]) => [card, resolveNickname(player)])} />
        <p>
          {state.yourTurn ? `Aluurm! Du bist dran!` : resolveNickname(state.turn) + " ist dran."}
          <br />
          <small>
            {resolveNickname(state.you.position)} <button onClick={(_) => {
              ws.send(JSON.stringify({
                action: "setname",
                name: prompt("Enter your name"),
              }))
            }}>Change Name</button>
          </small>
        </p>
        <span style={{ margin: '4em' }}>
          <Hand cards={state.you.cards} onClickCard={card => {
            console.log("clicked card", card)
            ws.send(JSON.stringify({
              action: "playcard",
              card,
            }))
          }} />
        </span>
      </section>
    </div>
  );
}
