import logo from './logo.svg';
import './App.css';
import React, { useEffect, useState } from 'react';
import { DEBUG_STATE, ICard, IGamePickingState, IReizState, ISkatPickingPhase, Stich } from './State';
import { Card, geileDeutschMap, geileFarbenMap, geileMap } from './Card';
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


const GamePickInput: React.FC<{ ws: WebSocket, state: IGamePickingState }> = ({ ws, state }) => {
  let dropdown = React.useRef<HTMLSelectElement>(null);

  if (!state.yourTurn)
    return <h1>{state.names[state.turn]} wählt das Spiel ...</h1>

  return (
    <div>
      <label>Was willst du spielen?</label>
      <br />
      <select ref={dropdown} defaultValue="ColorDiamonds">
        <option value="ColorDiamonds">♦</option>
        <option value="ColorHearts">♥</option>
        <option value="ColorSpades">♠</option>
        <option value="ColorClubs">♣</option>
        <option value="Null">Null</option>
        <option value="Grand">Grand</option>
      </select>
      <button onClick={() => {
        ws.send(JSON.stringify({
          action: "playvariant",
          variant: dropdown!.current!.value,
        }))
      }}>Spielen</button>
    </div>
  )
}



const SkatPickInput: React.FC<{ ws: WebSocket, state: ISkatPickingPhase }> = ({ ws, state }) => {
  if (!state.yourTurn)
    return <h1>{state.names[state.turn]} wählt den Skat ...</h1>
  return (
    <h1>Wähle deinen Skat ...</h1>
  )
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
  let sieSpielen: string = state.phase

  if (state.phase === "reizen") {
    sieSpielen = "Reizen ..."
  } else if (state.phase === "skatpicking") {
    sieSpielen = "Skat wählen ..."
  } else if (state.phase === "gamepicking") {
    sieSpielen = "Spiel wählen ..."
  } else if (state.phase === "running") {
    displayStich = state.currentStich.length === 0 ? state.lastStich : state.currentStich;
    sieSpielen = state.gamemode.kind
  } else if (state.phase === "finished") {
    displayStich = state.currentStich
    sieSpielen = "Nix mehr, das Spiel ist nämlich vorbei!"
  } else if (state.phase === "empty") {
    sieSpielen = "Noch nichts"
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

          <div style={{ width: '100%', fontSize: '.8em', background: 'grey', minWidth: '40vmin', minHeight: '40vmin', display: "flex", flexDirection: "column", justifyContent: "center", alignItems: "center" }}>
            {state.phase === "empty" ? <h1>Server ist down :(</h1> : null}
            {state.phase === "reizen" ? <ReizInput ws={ws} state={state} /> : null}
            {state.phase === "skatpicking" ? <SkatPickInput ws={ws} state={state} /> : null}
            {state.phase === "gamepicking" ? <GamePickInput ws={ws} state={state} /> : null}
            {state.phase === "running" || state.phase === "finished" ? <TableStack cards={displayStich.map(([card, player]) => [card, resolveNickname(player)])} /> : null}
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
            <small>
              {state.phase === 'running' && state.gamemode.kind === "Farbspiel"
                ? <span>
                  {!state.singlePlayer || state.singlePlayer === state.you.position
                    ? <span>Heute spielen Sie:</span>
                    : <span>Heute spielt {state.names[state.singlePlayer]}:</span>}
                  {' '}
                  <span style={{ color: geileFarbenMap[state.gamemode.color] }}>
                    {geileMap[state.gamemode.color]}
                  </span>{' '}
                  {geileDeutschMap[state.gamemode.color]}
                </span>
                : <span>Heute spielen Sie: {sieSpielen}</span>}
            </small>
          </header>

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
