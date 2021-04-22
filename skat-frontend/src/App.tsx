import logo from './logo.svg';
import './App.css';
import React, { useEffect, useState } from 'react';
import { DEBUG_STATE, ICard, Stich } from './State';
import { Card } from './Card';
import { Hand } from './Hand';
import { Scoreboard } from './Scoreboard';


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

  let displayStich:Stich = [];
  let whoIsDran = "Das hier darfst du nicht sehen!";
  let sieSpielen = "Noch nix"
  if (state.phase === "running") {
    whoIsDran = state.yourTurn ? `Aluurm! Du bist dran!` : resolveNickname(state.turn) + " ist dran."
    displayStich = state.currentStich.length === 0 ? state.lastStich : state.currentStich;
    sieSpielen = state.gamemode
  } else if (state.phase === "finished") {
    whoIsDran = resolveNickname(state.winner) + " hat gewonnen!"
    displayStich = state.currentStich
    sieSpielen = "Nix mehr, das Spiel ist nämlich vorbei!"
  }

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
          Wilkommen auf der Ramschinsel!
          <br />
          <small>Heute spielen Sie: {sieSpielen}</small>
        </header>

        <TableStack cards={displayStich.map(([card, player]) => [card, resolveNickname(player)])} />
        <p>
          {whoIsDran}
          <br />
          <small>
            {resolveNickname(state.you.position)} <button onClick={(_) => {
              let name = prompt("Enter your name")
              if(name) {
                ws.send(JSON.stringify({
                  action: "setname",
                  name,
                }))
                localStorage.setItem("nickname", name)
              }
            }}>Change Name</button>
          </small>
        </p>

        { state.phase === "finished" ? <Scoreboard state={state} /> : ""}
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
