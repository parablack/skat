import logo from './logo.svg';
import './App.css';
import React, { useEffect, useState } from 'react';
import { DEBUG_STATE, ICard, IReizState, Stich } from './State';
import { Card } from './Card';
import { YourHand, OpponentHands } from './Hand';
import { Scoreboard } from './Scoreboard';


const TableStack: React.FC<{ cards: [ICard, string][] }> = ({ cards }) => {
  return <div> {cards.length ? (
    <span style={{ display: 'flex', flexDirection: 'row', justifyContent: 'center' }}>
      {cards.map(([card, name], index) => {
        return <span style={{}} key={index}>
          <Card card={card} player={name}></Card>
        </span>
      })}
    </span>
  ) : (
    <img src={logo} className="App-logo" alt="logo" />
  )} </div>
}

const ReizInput: React.FC<{ ws: WebSocket, state: IReizState }> = ({ ws, state }) => {

  let textInput = React.useRef<HTMLInputElement>(null);

  if (!state.yourTurn) {
    return (
      <div style={{
        textAlign: 'center',
        verticalAlign: 'center'
      }}>
        <br />
        <br />
        <br />
                Es wurde schon {state.reizCurrentBid} geboten.
      </div>
    )
  }

  if (state.reizAnsagerTurn) {
    return (
      <div>
        Du darfst
        <br />
        <input ref={textInput}
          style={{ textAlign: "center" }}
          type="number"
          min={state.reizCurrentBid}
          max="100"
          size={5}
          defaultValue={state.reizCurrentBid + 1}
        />
        <button onClick={() => {
          ws.send(JSON.stringify({
            action: "reizbid",
            reizbid: parseInt(textInput!.current!.value),
          }))
        }}>bieten</button>
                oder
        <button onClick={() => {
          ws.send(JSON.stringify({
            action: "reizweg",
          }))
        }}>WEG!</button>
      </div>
    )
  } else {
    return (
      <div>
        Es wurde {state.reizCurrentBid} geboten:
        <br />
        <button onClick={() => {
          ws.send(JSON.stringify({
            action: "reizanswer", value: true
          }))
        }}>Ja</button>
                oder
        <button onClick={() => {
          ws.send(JSON.stringify({
            action: "reizanswer", value: false
          }))
        }}>Nein</button>
      </div>
    )
  }
}

export const App: React.FC<{ ws: WebSocket }> = ({ ws }) => {
  const [state, setState] = useState(DEBUG_STATE) /* TODO: remove DEBUG_STATE */
  // const [nickname, setNickname] = useState(undefined)

  let resolveNickname = (pos: string) => state.names[pos] || pos


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

  let displayStich: Stich = [];
  let sieSpielen = "Noch nix"

  if (state.phase === "reizen") {
    sieSpielen = "Gleich Skat ..."
  } else if (state.phase === "running") {
    displayStich = state.currentStich.length === 0 ? state.lastStich : state.currentStich;
    sieSpielen = state.gamemode
  } else if (state.phase === "finished") {
    displayStich = state.currentStich
    sieSpielen = "Nix mehr, das Spiel ist nämlich vorbei!"
  }

  return (
    <div className="App" style={{
      height: "100%",
    }}>
      <section style={{
        backgroundColor: '#282c34',
        display: 'flex',
        flexDirection: 'row',
        alignItems: 'center',
        justifyContent: 'space-evenly',
        fontSize: 'calc(10px + 2vmin)',
        color: 'white',
        height: '100%'
      }}>

        <div style={{
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'space-evenly',
          alignItems: 'center',
          height: '100%'
        }}>

          <span style={{ margin: '.4em' }}>
            <OpponentHands state={state} />
          </span>

          <div style={{ width: '100%', fontSize: '.8em', background: 'grey', minWidth: '40vmin', minHeight: '40vmin' }}>

            {state.phase === "empty" ? <h1>Server ist down :(</h1> : null}
            {state.phase === "reizen" ? <ReizInput ws={ws} state={state} /> : null}
            {state.phase === "reizen" ? <ReizInput ws={ws} state={state} /> : null}
            {state.phase === "running" ? <TableStack cards={displayStich.map(([card, player]) => [card, resolveNickname(player)])} /> : null}
          </div>

          {state.phase === "finished" ? <Scoreboard state={state} /> : ""}

          <span style={{ margin: '.4em' }}>
            <YourHand state={state} onClickCard={card => {
              console.log("clicked card", card)
              ws.send(JSON.stringify({
                action: "playcard",
                card,
              }))
            }} onChangeName={name => {
              ws.send(JSON.stringify({
                action: "setname",
                name
              }))
              localStorage.setItem("nickname", name)
            }} />
          </span>

        </div>

        <div style={{
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'space-evenly',
          alignItems: 'center',
          height: '100%'
        }}>
          <header>
            Wilkommen auf der Ramschinsel!
      <br />
            <small>Heute spielen Sie: {sieSpielen}</small>
          </header>

          { /*
    <div className="nameList">
    Ihre Mitspieler:
    <ul>
      { Object.entries(state.names).map(([pos, val]) =>
      <li key={pos.toString()}>{pos}: {val}</li>)}
    </ul>
    </div>
      */ }

          <div className="resign">
            Nächste Runde ({state.resign} / {Object.entries(state.names).length})
            <br />
            <button onClick={(_) => { ws.send(JSON.stringify({ action: "resign", })) }}>
              Aufgeben
            </button>
          </div>

        </div>

      </section>
    </div>
  );
}
