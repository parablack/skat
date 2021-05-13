import React from 'react';
import { ReizPhase, PickingPhase, PublicInfo, PrivateInfo } from './State';

export const ReizInput: React.FC<{ ws: WebSocket, phase: ReizPhase, publicInfo: PublicInfo, privateInfo?: PrivateInfo }> = ({ ws, phase, publicInfo, privateInfo }) => {
  let textInput = React.useRef<HTMLInputElement>(null);

  if (!privateInfo || !privateInfo.yourTurn) {
    return (
      <div style={{
        textAlign: 'center',
        verticalAlign: 'center'
      }}>
        Es wurde {phase.bid === 17 ? <>noch nix</> : phase.bid} geboten.
      </div>
    )
  }

  if (phase.isAnsagerTurn) {
    return (
      <div>
        Du darfst
        <br />
        <div style={{ display: "flex", flexDirection: "row", justifyContent: "center", alignItems: "center" }}>
          <input ref={textInput}
            style={{ textAlign: "center" }}
            type="number"
            min={phase.bid}
            max="264"
            size={5}
            defaultValue={phase.bid + 1}
          />
          <button className="unicode-button" onClick={() => {
            ws.send(JSON.stringify({
              action: "reizbid",
              reizbid: parseInt(textInput!.current!.value),
            }))

          }}>📈</button>
          oder
          <button className="unicode-button" onClick={() => {
            ws.send(JSON.stringify({
              action: "reizweg",
            }))
          }}>📉</button>
        </div>
      </div>
    )
  } else {
    return (
      <div>
        Es wurde {phase.bid} geboten:
        <br />
        <div style={{ display: "flex", flexDirection: "row", justifyContent: "center", alignItems: "center" }}>
          <button className="unicode-button" onClick={() => {
            ws.send(JSON.stringify({
              action: "reizanswer", value: true
            }))
          }}>&#x1F44D;</button>
            oder
          <button className="unicode-button" onClick={() => {
            ws.send(JSON.stringify({
              action: "reizanswer", value: false
            }))
          }}>&#x1F44E;</button>
        </div>
      </div>
    )
  }
}



export const PickingInput: React.FC<{ ws: WebSocket, phase: PickingPhase, publicInfo: PublicInfo, privateInfo?: PrivateInfo }> = ({ ws, phase, publicInfo, privateInfo }) => {
  let dropdown = React.useRef<HTMLSelectElement>(null);
  let dropdown2 = React.useRef<HTMLSelectElement>(null);

  if (!privateInfo || !privateInfo.yourTurn) {
    return <h1>
      {publicInfo.names[publicInfo.turn!] || publicInfo.turn!}
      {' '} wählt {{
        DiscardingSkat: 'den Skat',
        PickingHand: '✋/nicht ✋',
        PickingGamemode: 'das Spiel'
      }[phase.subPhase]}
      ...
    </h1>
  }

  if (phase.subPhase === 'PickingGamemode') {
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
        <select ref={dropdown2} defaultValue="Normal">
          <option value="Normal">Normal</option>
          <option value="Schneider">Schneider</option>
          <option value="Schwarz">Schwarz</option>
          <option value="Ouvert">Ouvert</option>
        </select>
        <button onClick={() => {
          ws.send(JSON.stringify({
            action: "playvariant",
            variant: dropdown!.current!.value,
            angesagt: dropdown2!.current!.value,
          }))
        }}>Spielen</button>
      </div>
    )
  } else if (phase.subPhase === 'DiscardingSkat') {
    return <h1>Wähle deinen Skat ...</h1>
  } else {
    return (
      <div>
        <button onClick={() => {
          ws.send(JSON.stringify({
            action: "playhand",
            hand: true,
          }))
        }}>Hand Spielen</button>
        {" oder "}
        <button onClick={() => {
          ws.send(JSON.stringify({
            action: "playhand",
            hand: false,
          }))
        }}>Hand nicht Spielen</button>
      </div>
    )
  }
}
