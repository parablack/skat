import logo from './logo.svg';
import './App.css';
import React, { useEffect, useState } from 'react';
import { EMPTY_STATE, Card as ICard, Stich, Phase, PublicInfo, PrivateInfo } from './State';
import { geileDeutschMap, geileFarbenMap, geileMap } from './Cards/SimpleCard';
import { Card } from './Cards/ImageCard';
import { YourHand, OpponentHands } from './Hand';
import { Scoreboard } from './Scoreboard';
import { ReizInput, PickingInput } from './Reizen';
import { LobbyInput } from './Lobby';


const TableStack: React.FC<{ cards: [ICard, string][] }> = ({ cards }) => {
  if (!cards.length)
    return <img src={logo} className="App-logo" alt="logo" />

  return <span style={{ display: 'flex', flexDirection: 'row', justifyContent: 'center' }}>
    {cards.map(([card, name], index) => {
      return <span key={index} style={{ display: 'flex', flexDirection: 'column', justifyContent: 'center' }}>
        <span style={{ margin: '0.5em' }}>
          <Card card={card} />
        </span>
        <small>
          {name}
        </small>
      </span>
    })}
  </span>
}

export const SieSpielen: React.FC<{ phase: Phase, publicInfo: PublicInfo, privateInfo?: PrivateInfo }> = ({ phase, publicInfo, privateInfo }) => {
  let sieSpielen: string = phase.phase
  if (phase.phase === "ReizPhase") {
    sieSpielen = "Reizen ..."
  } else if (phase.phase === "PickingPhase") {
    if (phase.subPhase === 'DiscardingSkat')
      sieSpielen = "Skat wählen ..."
    else if (phase.subPhase === 'PickingGamemode')
      sieSpielen = "Spiel wählen ..."
    else
      sieSpielen = "Hand wählen ..."
  } else if (phase.phase === "RunningPhase") {
    sieSpielen = phase.gameMode.kind
  } else if (phase.phase === "FinishedPhase") {
    sieSpielen = "Nix mehr, das Spiel ist nämlich vorbei!"
  }
  return <header>
    🏝️Ramschinsel🏝️
    <br />
    <small>
      {phase.phase === 'RunningPhase'
        ? <span>
          {!phase.singlePlayer || phase.singlePlayer === privateInfo?.yourPosition
            ? <span>Heute spielen Sie:</span>
            : <span>Heute spielt {publicInfo.names[phase.singlePlayer] || phase.singlePlayer}:</span>}
          {' '}
          {phase.gameMode.kind === "Farbspiel" ? <>
            <span style={{ color: geileFarbenMap[phase.gameMode.color] }}>
              {geileMap[phase.gameMode.color]}
            </span>{' '}
            {geileDeutschMap[phase.gameMode.color]}</>
            : phase.gameMode.kind}
          {' '}
          {phase.scoring.hand ? "Hand" : " "}{' '}{phase.scoring.angesagt !== 'Normal' ? phase.scoring.angesagt : null}
        </span>
        : <span>Heute spielen Sie: {sieSpielen}</span>}
    </small>
  </header>
}

export const GameInput: React.FC<{ ws: WebSocket, phase: Phase, publicInfo: PublicInfo, privateInfo?: PrivateInfo }> = ({ ws, phase, publicInfo, privateInfo }) => {
  let resolveNickname = (pos: string) => publicInfo.names[pos] || pos
  let displayStich: Stich = [];
  if (phase.phase === "RunningPhase") {
    displayStich = phase.currentStich.length === 0 ? phase.lastStich : phase.currentStich;
  } else if (phase.phase === "FinishedPhase") {
    displayStich = phase.currentStich
  }

  return (
    <div className="App" style={{
      height: "100%",
      overflow: 'hidden',
    }}>
      <section style={{
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
          height: '100%',
          width: '50vw',
        }}>
          {privateInfo !== undefined ? (
            <OpponentHands
              publicInfo={publicInfo}
              ownPosition={privateInfo.yourPosition}
              onChangePos={(pos) => ws.send(
                JSON.stringify({ action: "changepos", position: pos })
              )}
              statusElement={<SieSpielen phase={phase} publicInfo={publicInfo} />}
            />
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
              {phase.phase === "ReizPhase" ? <ReizInput ws={ws} phase={phase} publicInfo={publicInfo} privateInfo={privateInfo} /> : null}
              {phase.phase === "PickingPhase" ? <PickingInput ws={ws} phase={phase} publicInfo={publicInfo} privateInfo={privateInfo} /> : null}
              {phase.phase === "RunningPhase" || phase.phase === 'FinishedPhase'
                ? <TableStack cards={displayStich.map(([card, player]) => [card, resolveNickname(player)])} />
                : null}
            </div>
            <span style={{ width: '4em', display: 'flex', flexDirection: 'column', justifyContent: 'center' }}>
              <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "showcards", })) }}>👀</button>
              <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "resign", })) }}>
                <div style={{ position: 'relative', overflow: 'hidden', width: '100%', height: '100%' }}>
                  <span style={{ position: 'absolute', left: '50%', top: '50%', transform: 'translate(-50%, -50%)' }}>🏳️</span>
                  <span style={{ position: 'absolute', left: '50%', top: '50%', transform: 'translate(-40%, -85%)', fontSize: '.5em', color: 'black' }}>
                    {publicInfo.numResigned}/{Object.entries(publicInfo.names).length}
                  </span>
                </div>
              </button>
              <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "leave", })) }}>🚪</button>
            </span>
          </div>

          {phase.phase === "FinishedPhase" ? <Scoreboard phase={phase} publicInfo={publicInfo} /> : ""}

          {privateInfo ? (
            <span style={{ margin: '.4em' }}>
              <YourHand
                publicInfo={publicInfo}
                privateInfo={privateInfo}
                onClickCard={card => {
                  console.log("clicked card", card)
                  ws.send(JSON.stringify({
                    action: "playcard",
                    card,
                  }))
                }} />
            </span>
          ) : null}
        </div>
      </section>
    </div>
  );
}

export const App: React.FC<{ ws: WebSocket }> = ({ ws }) => {
  const [state, setState] = useState(EMPTY_STATE)


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


  if (state.type === 'empty') {
    return <div style={{
      display: 'flex',
      justifyContent: 'center',
      width: '100%',
      height: '100%',
    }}>
      <h1>
        Server ist down
        <button className="unicode-button" onClick={() => window.location.reload()}>
          &#x1F62D;
        </button>
      </h1>
    </div>
  } else if (state.type === "lobby") {
    return <LobbyInput ws={ws} state={state} />
  } else if (state.type === 'playerState') {
    return <GameInput ws={ws} phase={state.phase} publicInfo={state.public} />
  } else {
    return <GameInput ws={ws} phase={state.phase} publicInfo={state.public} privateInfo={state.private} />
  }
}
