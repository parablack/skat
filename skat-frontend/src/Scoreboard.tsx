import { IFinishedState } from "./State"

export const Scoreboard: React.FC<{ state: IFinishedState }> = ({ state }) => {

    let sum = Object.values(state.scores).reduce((x, y) => x + y, 0)

    let resultName = state.names[state.result[0]] || "Alarm?"
    let resultWon = state.result[1];
    let resultPoints = state.result[2];

    return <div>
        {resultName} hat {resultWon ? <>gewonnen</> : <>verloren 😢</>}!
        <br /> Dies gibt {resultPoints} Punkte!
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