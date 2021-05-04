import React from 'react';
import { IGamePickingState, IHandPickingPhase, IReizState, ISkatPickingPhase } from './State';

export const ReizInput: React.FC<{ ws: WebSocket, state: IReizState }> = ({ ws, state }) => {

    let textInput = React.useRef<HTMLInputElement>(null);

    if (!state.yourTurn) {
        return (
            <div style={{
                textAlign: 'center',
                verticalAlign: 'center'
            }}>
                Es wurde {state.reizCurrentBid === 17 ? <>noch nix</> : state.reizCurrentBid} geboten.
            </div>
        )
    }

    if (state.reizAnsagerTurn) {
        return (
            <div>
                Du darfst
                <br />
                <div style={{ display: "flex", flexDirection: "row", justifyContent: "center", alignItems: "center" }}>
                    <input ref={textInput}
                        style={{ textAlign: "center" }}
                        type="number"
                        min={state.reizCurrentBid}
                        max="264"
                        size={5}
                        defaultValue={state.reizCurrentBid + 1}
                    />
                    <button className="unicode-button" onClick={() => {
                        ws.send(JSON.stringify({
                            action: "reizbid",
                            reizbid: parseInt(textInput!.current!.value),
                        }))

                    }}>&#x1F4C8;</button>
          oder
          <button className="unicode-button" onClick={() => {
                        ws.send(JSON.stringify({
                            action: "reizweg",
                        }))
                    }}>&#x1F6AA;</button>
                </div>
            </div>
        )
    } else {
        return (
            <div>
                Es wurde {state.reizCurrentBid} geboten:
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


export const HandPickInput: React.FC<{ ws: WebSocket, state: IHandPickingPhase }> = ({ ws, state }) => {
    if (!state.yourTurn)
        return <h1>{state.names[state.turn]} wählt Hand/nicht Hand ...</h1>

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

export const GamePickInput: React.FC<{ ws: WebSocket, state: IGamePickingState }> = ({ ws, state }) => {
    let dropdown = React.useRef<HTMLSelectElement>(null);
    let dropdown2 = React.useRef<HTMLSelectElement>(null);

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
}



export const SkatPickInput: React.FC<{ ws: WebSocket, state: ISkatPickingPhase }> = ({ ws, state }) => {
    if (!state.yourTurn)
        return <h1>{state.names[state.turn]} wählt den Skat ...</h1>
    return (
        <h1>Wähle deinen Skat ...</h1>
    )
}