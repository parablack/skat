import React from "react";
import { Card } from "./Cards/ImageCard";
import { Card as ICard, Player, PrivateInfo, PublicInfo } from "./State";

const ratio = 6 / 9.25   // ratio cardWidth/cardHeight (including colored border)

function nextPlayer(s: Player): Player {
    if (s === "Geber") return "Vorhand";
    else if (s === "Vorhand") return "Mittelhand";
    else if (s === "Mittelhand") return "Geber";
    else return s // s: never
}


function createPlayerStruct(state: PublicInfo, pos: Player) {
    return {
        cards: state.cards[pos],
        name: state.names[pos],
        position: pos,
        active: state.turn === pos
    }
}

// magic math follows

function f(t: number, b: number, theta: number) {

    const cosT = Math.cos(theta);
    const sinT = Math.sin(theta);

    // reparametrization

    let r = Math.sqrt(4 - 2 * cosT);

    let alpha = 0;
    alpha += Math.atan2(2 - r, - 2 * cosT) * t
    alpha -= Math.atan2(2 - r, 2 * cosT) * (t - 1)
    alpha += Math.atan2(2 + r, 2 * cosT) * (t - 1)
    alpha -= Math.atan2(2 + r, - 2 * cosT) * t

    let d = 1;
    d *= 2 - cosT - 2 * r + 2 + 2 * cosT * cosT;
    d /= 2 - cosT + 2 * r + 2 + 2 * cosT * cosT;

    let zr = -2 * r * Math.sin(alpha) * Math.sqrt(d);
    let zi = (-2 - r) * d + r + 4 * Math.cos(alpha) * Math.sqrt(d) - 2;
    let zn = Math.sqrt(zr * zr + zi * zi)

    let cosH = zr / zn;
    let sinH = zi / zn;

    // actual function: H = s * pi, 0 < s < 1

    let fr = 2 * cosT * sinH + 2;
    let fx = b * (cosT * sinH + 1) - b * cosH;
    let fy = b * sinH * sinT;

    let dfr = 2 * cosT * cosH;
    let dfx = b * (cosT * cosH) + b * sinH;
    let dfy = b * cosH * sinT;

    let xx = (dfx * fr - fx * dfr) / fr / fr;
    let yy = (dfy * fr - fy * dfr) / fr / fr;

    let rot = -Math.atan2(yy, xx);
    return [fx / fr, fy / fr, rot];
}

interface IHandProps {
    cards: ICard[],
    onClickCard?: (card: ICard) => void,
    theta: number,
    overlap: number,
    scale: number
}

export const Hand: React.FC<IHandProps> = ({ cards, onClickCard, theta, overlap, scale }) => {
    return (<div style={{
        display: 'flex',
        justifyContent: 'center',
        fontSize: '.8em',
        width: (overlap * scale * 10 * 7) + "rem"
    }}>
        {
            cards.map((card, index) => {
                let width = cards.length * overlap * scale;
                let a = (cards.length - width) / 2; // leftmost x position of arc
                let b = (cards.length + width) / 2; // rightmost x position of arc
                let t = (index + 0.5) / cards.length;   // position on arc between 0 and 1
                let [x, y, r] = f(t, b - a, theta);
                x += a
                y -= f(0.5, b - a, theta)[1];
                return (
                    <span key={index} style={{
                        transform: `translate(${(x - index - 0.5) * 100}%, ${-ratio * y * 100}%) rotate(${r}rad) scale(${scale})`,
                        transformOrigin: 'center center',
                        cursor: onClickCard ? "pointer" : "default",
                    }}>
                        <Card card={card} onClick={() => { if (onClickCard) { onClickCard(card) } }}></Card>
                    </span>)
            })
        }
    </div>)
}


export const YourHand: React.FC<{ publicInfo: PublicInfo, privateInfo: PrivateInfo, onClickCard: (card: ICard) => void }> = ({ publicInfo, privateInfo, onClickCard }) => {
    return <div>
        <Hand
            cards={privateInfo.yourCards}
            onClickCard={onClickCard}
            theta={27 * Math.PI / 180}
            overlap={0.5}
            scale={1}
        />
        <div style={{
            color: privateInfo.yourTurn ? "red" : "white",
            margin: "0.5em"
        }}>
            <div>{publicInfo.names[privateInfo.yourPosition] || privateInfo.yourPosition}</div>
            <div><small>{privateInfo.yourPosition}</small></div>
        </div>
    </div>
}



export const OpponentHands: React.FC<{
    publicInfo: PublicInfo,
    ownPosition: Player,
    onChangePos: (pos: String) => void,
    statusElement: React.ReactNode
}> = ({ publicInfo, onChangePos, statusElement, ownPosition }) => {
    let left = createPlayerStruct(publicInfo, nextPlayer(ownPosition))
    let right = createPlayerStruct(publicInfo, nextPlayer(nextPlayer(ownPosition)))
    return <div style={{
        display: 'flex',
        justifyContent: 'flex-start',
        fontSize: '.8em',
        height: "8rem",
        width: "100%",
        position: "relative"
    }}>
        <div style={{
            position: "absolute",
            left: "0",
            flexDirection: "column",
            color: left.active ? "red" : "white"
        }}>
            {left.name
                ? <div>{left.name}</div>
                : <button onClick={() => {
                    onChangePos(left.position)
                }}>Mein Platz!</button>
            }
            <div><small>{left.position}</small></div>
        </div>
        <span style={{
            width: '100%',
            height: '100%',
            display: 'flex',
            alignItems: 'center',
            flexDirection: 'column',
            justifyContent: 'flex-end',
        }}>
            {statusElement}
        </span>
        <div style={{
            position: "absolute",
            right: "0",
            flexDirection: "column",
            color: right.active ? "red" : "white"
        }}>
            {right.name
                ? <div>{right.name}</div>
                : <button onClick={() => {
                    onChangePos(right.position)
                }}>Mein Platz!</button>
            }
            <div><small>{right.position}</small></div>
        </div>
        <div style={{
            position: "absolute",
            left: "0",
            transformOrigin: "center center",
            transform: `translate(-15%, 15%) rotate(${180 - 20}deg)`,
        }}>
            {<Hand
                cards={left.cards}
                theta={27 * Math.PI / 180}
                overlap={0.4}
                scale={0.55}
            ></Hand>}
        </div>
        <div style={{
            position: "absolute",
            right: "0",
            transformOrigin: "center center",
            transform: `translate(15%, 15%) rotate(${180 + 20}deg)`,
        }}>
            {<Hand
                cards={right.cards}
                theta={27 * Math.PI / 180}
                overlap={0.4}
                scale={0.55}
            ></Hand>}
        </div>
    </div>
}
