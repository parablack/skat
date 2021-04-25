import React from "react";
import { Card } from "./Card";
import { ICard } from "./State";

const ratio = 84.9667 / 122.567   // ratio cardWidth/cardHeight (including colored border)
const overlap = 0.5;              // between 0 and 1, smaller = larger overlap


// magic math follows

function f(t: number, b: number, theta: number) {

    const cosT = Math.cos(theta);
    const sinT = Math.sin(theta);

    // reparametrization

    let r = Math.sqrt(4 - 2 * cosT);

    let alpha = 0;
    alpha += Math.atan2(2 - r, - 2 * cosT) * t
    alpha -= Math.atan2(2 - r,   2 * cosT) * (t - 1)
    alpha += Math.atan2(2 + r,   2 * cosT) * (t - 1)
    alpha -= Math.atan2(2 + r, - 2 * cosT) * t

    let d = 1;
    d *= 2 - cosT - 2 * r + 2 + 2 * cosT * cosT;
    d /= 2 - cosT + 2 * r + 2 + 2 * cosT * cosT;

    let zr = -2 * r * Math.sin(alpha) * Math.sqrt(d);
    let zi = (-2 - r) * d + r + 4* Math.cos(alpha) * Math.sqrt(d) - 2;
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


export const YourHand: React.FC<{ cards: ICard[], onClickCard: (card: ICard) => void }> = ({ cards, onClickCard }) => {
    let theta = 27 * Math.PI / 180; // inner angle of the arc (unit: rad)

    return <div style={{ display: 'flex', justifyContent: 'center', fontSize: '.8em', maxWidth: (overlap * 10 * 7)+"rem" }}>
        {
            cards.map((card, index) => {
                let width = cards.length * overlap;
                let a = (cards.length - 1 - width) / 2; // leftmost x position of arc
                let b = (cards.length - 1 + width) / 2; // rightmost x position of arc

                let t = (index + 0.5) / cards.length; // position on arc between 0 and 1

                let [x, y, r] = f(t, b - a, theta);
                x += a
                y -= f(0.5, b - a, theta)[1] / 2;


                return <span key={index} style={{
                    transform: `translate(${(x - index) * 100}%, ${-ratio * y * 100}%) rotate(${r}rad)`,
                    transformOrigin: 'center center',
                }}>
                    <Card card={card} onClick={() => onClickCard(card)}></Card>
                </span>
            })}
    </div>
}

export const OpponentHands: React.FC<{ left: ICard[], right: ICard[] }> = ({ left, right }) => {
    return <div style={{
        display: 'flex',
        justifyContent: 'flex-start', fontSize: '.8em', width: (overlap * 10 * 7)+"rem",
        position: "relative"
    }}>
    <div style={{
        position: "absolute",
        left: "0",
        display: "flew",
        flexDirection: "column"
    }}>
        <div>Opponent 1</div>
        <div>(Role 1)</div>
    </div>
    <div style={{
        position: "absolute",
        right: "0",
        display: "flew",
        flexDirection: "column"
    }}>
        <div>Opponent 2</div>
        <div>(Role 2)</div>
    </div>
    <div style={{
        display: 'flex',
        transformOrigin: "bottom left",
        transform: "translate(7%, -270%) rotate(140deg)"
    }}>
    {
        left.map((card, index) => {
            let theta = 89.9999 * Math.PI / 180;
            let width = overlap * left.length / 2;
            let a = (left.length - 1 - width) / 2; // leftmost x position of arc
            let b = (left.length - 1 + width) / 2; // rightmost x position of arc
            let t = (index + 0.5) / left.length; // position on arc between 0 and 1
            let [x, y, r] = f(t, b - a, theta);
            // y -= f(0.5, b - a, theta)[1] / 2;
            // x = 0;
            y += 1.5;
            // r = 0;


            return <span style={{
                transform: `translate(${(x - index) * 100}%, ${-ratio * y * 100}%) rotate(${r}rad)`,
                transformOrigin: 'center center',
                // <Card card={card} onClick={() => 0}></Card>
            }}>

            </span>
        })}
        </div>
    </div>
}
