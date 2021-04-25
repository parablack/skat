import React from "react";
import { Card } from "./Card";
import { ICard } from "./State";


const ratio = 84.9667 / 122.567   // ratio cardWidth/cardHeight (including colored border)
const overlap = 0.5;              // between 0 and 1, smaller = larger overlap
const theta = 27 * Math.PI / 180; // inner angle of the arc (unit: rad)

const cosT = Math.cos(theta);
const sinT = Math.sin(theta);

// magic math follows

function f(t: number, b: number) {

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


export const Hand: React.FC<{ cards: ICard[], onClickCard: (card: ICard) => void }> = ({ cards, onClickCard }) => {
    return <div style={{ display: 'flex', justifyContent: 'center', fontSize: '.8em', maxWidth: (overlap * 10 * 7)+"rem" }}>
        {
            cards.map((card, index) => {
                let width = cards.length * overlap;
                let a = (cards.length - 1 - width) / 2; // leftmost x position of arc
                let b = (cards.length - 1 + width) / 2; // rightmost x position of arc

                let t = (index + 0.5) / cards.length; // position on arc between 0 and 1

                let [x, y, r] = f(t, b - a);
                x += a
                y -= f(0.5, b - a)[1] / 2;


                return <span key={index} style={{
                    transform: `translate(${(x - index) * 100}%, ${-ratio * y * 100}%) rotate(${r}rad)`,
                    transformOrigin: 'center center',
                    width: ""
                }}>
                    <Card card={card} onClick={() => onClickCard(card)}></Card>
                </span>
            })}
    </div>
}
