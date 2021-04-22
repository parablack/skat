import React from "react";
import { Card } from "./Card";
import { ICard } from "./State";


const ratio = 84.9667 / 122.567   // ratio cardWidth/cardHeight (including colored border)
const width = 6;                  // with of the arc (unit: card widths)
const theta = 27 * Math.PI / 180; // inner angle of the arc (unit: rad)

// magic math follows
const h = 1e-8;
const n = 500;

function f(t: number, a: number, b: number) {
    let w1 = Math.sin(theta)
    let w2 = Math.cos(theta)
    let z1 = Math.cos(t * Math.PI)
    let z2 = Math.sin(t * Math.PI)
    let x = ((a + b) * (w2 * z2 + 1) + (a - b) * z1) / (2 * w2 * z2 + 2)
    let y = (b - a) * w1 * z2 / (2 * w2 * z2 + 2)
    return [x, y]
}

function df(t: number, a: number, b: number) {
    let [x, y] = f(t, a, b)
    let [x2, y2] = f(t + h, a, b);
    let [dx, dy] = [(x2 - x) / h, (y2 - y) / h];
    return [dx, dy];
}

function arclengthParam(t: number, a_: number, b_: number) {
    let res = 0;
    let cache = []; // TODO compute once outside loop
    cache.length = n;
    for (let i = 0; i < n; i++) {
        let [ax, ay] = df(i / n, a_, b_);
        let a = Math.sqrt(ax * ax + ay * ay);
        let [bx, by] = df((i + 1) / n, a_, b_);
        let b = Math.sqrt(bx * bx + by * by);
        res += 0.5 * (a + b) / n;
        cache[i] = res;
    }
    for (let i = 0; i < n; i++) {
        if (cache[i] > t * res) {
            return (i) / n;
        }
    }
    return 1;
}

export const Hand: React.FC<{ cards: ICard[] }> = ({ cards }) => {
    return <div style={{ display: 'flex', fontSize: '.8em', width: 'var(--cards-width)' }}>
        {
            cards.map((card, index) => {
                let a = (cards.length - 1 - width) / 2; // leftmost x position of arc
                let b = (cards.length - 1 + width) / 2; // rightmost x position of arc

                let t = index / (cards.length - 1); // position on arc between 0 and 1
                let u = arclengthParam(t, a, b);
                let [x, y] = f(u, a, b);
                let [dx, dy] = df(u, a, b);
                let r = -Math.atan2(dy, dx);

                return <span style={{
                    transform: `translate(${(x - index) * 100}%, ${-ratio * y * 100}%) rotate(${r}rad)`,
                    transformOrigin: 'center center',
                    width: ""
                }}>
                    <Card card={card} onClick={() => alert("aluurm " + JSON.stringify(card))}></Card>
                </span>
            })}
    </div>
}
