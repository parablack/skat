import { IFinishedState } from "./State"

export const Scoreboard: React.FC<{ state: IFinishedState }> = ({ state }) => {

    let sum = Object.values(state.scores).reduce((x, y) => x + y, 0)

    let namenWin = (state.winner.map(x => state.names[x])).join(" und ")

    return <div>
        {namenWin} {state.winner.length === 1 ? <>hat</> : <>haben</>} gewonnen!
        <table>
            <tbody>
                {
                    Object.entries(state.scores).map(([name, score]) =>
                     <tr>
                        <td>{state.names[name] || name}</td>
                        <td>{score}</td>
                    </tr> )
                }
                 <tr>
                        <td><i>Im Skat übrig</i></td>
                        <td><i>{120-sum}</i></td>
                    </tr>
            </tbody>
        </table>
    </div>
}