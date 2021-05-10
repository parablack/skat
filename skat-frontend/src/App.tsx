import logo from './logo.svg';
import './App.css';
import React, { useEffect, useState } from 'react';
import { EMPTY_STATE, ICard, inLobby, Stich } from './State';
import { geileDeutschMap, geileFarbenMap, geileMap } from './Cards/SimpleCard';
import { Card } from './Cards/ImageCard';
import { YourHand, OpponentHands } from './Hand';
import { Scoreboard } from './Scoreboard';
import { ReizInput, HandPickInput, SkatPickInput, GamePickInput } from './Reizen';
import { LobbyInput } from './Lobby';


const TableStack: React.FC<{ cards: [ICard, string][] }> = ({ cards }) => {
  return <div> {cards.length ? (
    <span style={{ display: 'flex', flexDirection: 'row', justifyContent: 'center' }}>
      {cards.map(([card, name], index) => {
        return <>
          <span style={{ display: 'flex', flexDirection: 'column', justifyContent: 'center' }}>
            <span style={{ margin: '0.5em' }} key={index}>
              <Card card={card} player={name}></Card>
            </span>
            <small>
              {name}
            </small>
          </span>
        </>
      })}
    </span>
  ) : (
    <img src={logo} className="App-logo" alt="logo" />
  )} </div>
}


export const App: React.FC<{ ws: WebSocket }> = ({ ws }) => {
  const [state, setState] = useState(EMPTY_STATE)
  // const [nickname, setNickname] = useState(undefined)

  let resolveNickname = (pos: string) => inLobby(state) ? state.names[pos] || pos : pos


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

    ws.onclose = () => {
      console.log("ws closed")
      setState(EMPTY_STATE)
    }

    return () => ws.close()
  }, [ws])

  if (state.phase === "lobby") {
    return <LobbyInput ws={ws} state={state} />
  }

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
  } else if (state.phase === "handpicking") {
    sieSpielen = "Hand wählen ..."
  }

  return (
    <div className="App" style={{
      height: "100%",
      overflow: 'hidden',
    }}>
      <section style={{
        // backgroundColor: '#282c34',
        display: 'flex',
        flexDirection: inLobby(state) ? 'row' : 'column-reverse',
        alignItems: 'center',
        justifyContent: 'space-evenly',
        fontSize: 'calc(10px + 2vmin)',
        color: 'white',
        height: '100%'
      }}>

        <div style={{
          display: 'flex',
          flexDirection: 'column',
          justifyContent: inLobby(state) ? 'space-evenly' : 'normal',
          alignItems: 'center',
          height: '100%',
          width: '50vw',
        }}>

          {inLobby(state) ? (
            <span style={{ margin: '.4em' }}>
              <OpponentHands
                state={state}
                onChangePos={
                  (pos) => ws.send(
                    JSON.stringify({ action: "changepos", position: pos })
                  )}
                statusElement={
                  <header>
                    🏝️Ramschinsel🏝️
                    <br />
                    <small>
                      {state.phase === 'running'
                        ? <span>
                          {!state.singlePlayer || state.singlePlayer === 'nobody' || state.singlePlayer === state.you.position
                            ? <span>Heute spielen Sie:</span>
                            : <span>Heute spielt {state.names[state.singlePlayer] || state.singlePlayer}:</span>}
                          {' '}
                          {state.gamemode.kind === "Farbspiel" ? <>
                            <span style={{ color: geileFarbenMap[state.gamemode.color] }}>
                              {geileMap[state.gamemode.color]}
                            </span>{' '}
                            {geileDeutschMap[state.gamemode.color]}</>
                            : state.gamemode.kind}
                          {' '}
                          {state.scoring.hand ? "Hand" : " "}{' '}{state.scoring.angesagt !== 'Normal' ? state.scoring.angesagt : null}
                        </span>
                        : <span>Heute spielen Sie: {sieSpielen}</span>}
                    </small>
                  </header>
                }
              />
            </span>
          ) : null}

          <div style={{
            display: "flex",
            flexDirection: "row",
            width: '100%',
            fontSize: '.8em',
          }}>
            <span style={{ width: '4em' }}></span>
            <div style={{
              width: '100%',
              boxShadow: "inset 0 0 5em 2.5em #252525",
              background: "#404040",
              minWidth: '40vmin',
              minHeight: '40vmin',
              display: "flex",
              flexDirection: "column",
              justifyContent: "center",
              alignItems: "center"
            }}>
              {state.phase === "empty" ? <h1>Server ist down <button className="unicode-button" onClick={() => window.location.reload()}>&#x1F62D;</button></h1> : null}
              {state.phase === "reizen" ? <ReizInput ws={ws} state={state} /> : null}
              {state.phase === "handpicking" ? <HandPickInput ws={ws} state={state} /> : null}
              {state.phase === "skatpicking" ? <SkatPickInput ws={ws} state={state} /> : null}
              {state.phase === "gamepicking" ? <GamePickInput ws={ws} state={state} /> : null}
              {state.phase === "running" || state.phase === "finished" ? <TableStack cards={displayStich.map(([card, player]) => [card, resolveNickname(player)])} /> : null}
            </div>
            <span style={{ width: '4em', display: 'flex', flexDirection: 'column', justifyContent: 'center' }}>
              <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "showcards", })) }}>👀</button>
              {inLobby(state) ? (
                <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "resign", })) }}>
                  <div style={{ position: 'relative', overflow: 'hidden', width: '100%', height: '100%' }}>
                    <span style={{ position: 'absolute', left: '50%', top: '50%', transform: 'translate(-50%, -50%)' }}>🏳️</span>
                    <span style={{ position: 'absolute', left: '50%', top: '50%', transform: 'translate(-40%, -85%)', fontSize: '.5em', color: 'black' }}>
                      {state.resign}/{Object.entries(state.names).length}
                    </span>
                  </div>
                </button>
              ) : null}
              <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "leave", })) }}>🚪</button>
            </span>
          </div>

          {state.phase === "finished" ? <Scoreboard state={state} /> : ""}

          {inLobby(state) ? (
            <span style={{ margin: '.4em' }}>
              <YourHand state={state} onClickCard={card => {
                console.log("clicked card", card)
                ws.send(JSON.stringify({
                  action: "playcard",
                  card,
                }))
              }} />
            </span>
          ) : null}

        </div>

        {/*
        <div style={{
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'space-evenly',
          alignItems: 'center',
          height: '100%'
        }}>
          <header>
            Wilkommen auf der 🏝️Ramschinsel🏝️!
            <br />
            <small>
              {state.phase === 'running'
                ? <span>
                  {!state.singlePlayer || state.singlePlayer === 'nobody' || state.singlePlayer === state.you.position
                    ? <span>Heute spielen Sie:</span>
                    : <span>Heute spielt {state.names[state.singlePlayer]}:</span>}
                  {' '}
                  {state.gamemode.kind === "Farbspiel" ? <>
                    <span style={{ color: geileFarbenMap[state.gamemode.color] }}>
                      {geileMap[state.gamemode.color]}
                    </span>{' '}
                    {geileDeutschMap[state.gamemode.color]}</>
                    : state.gamemode.kind}
                  {' '}
                  {state.scoring.hand ? "Hand" : " "}{' '}{state.scoring.angesagt !== 'Normal' ? state.scoring.angesagt : null}
                </span>
                : <span>Heute spielen Sie: {sieSpielen}</span>}
            </small>

          </header>

          {inLobby(state) ? <>
            <div>
              Name ändern <br />
              <button className="unicode-button" onClick={(_) => {
                let name = prompt("Enter your name")
                if (name) {
                  ws.send(JSON.stringify({
                    action: "setname",
                    name
                  }))
                  localStorage.setItem("nickname", name)
                }
              }
              }>
                ✏️
              </button>
            </div>

            <div>
              Mit offenen Karten spielen
              <br />
              <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "showcards", })) }}>
                👀
              </button>
            </div>

            <div className="resign">
              Nächste Runde ({state.resign} / {Object.entries(state.names).length})
              <br />
              <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "resign", })) }}>
                🇫🇷
              </button>
            </div>
          </> : null}
        </div>
        */}
      </section>
    </div>
  );
}
