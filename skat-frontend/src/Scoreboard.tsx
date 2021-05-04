import { IFinishedState } from "./State"

export const Scoreboard: React.FC<{ state: IFinishedState }> = ({ state }) => {

    let sum = Object.values(state.scores).reduce((x, y) => x + y, 0)

    let resultName = state.names[state.result[0]] || state.result[0]
    let resultWon = state.result[1];
    let resultPoints = state.result[2];
    let resultUeberreizt = state.result[3];

    return <div>
        {resultName} hat {resultWon ? <>gewonnen</> :
            resultUeberreizt ? <>sich überreizt 😢😢😢</> :  <>verloren 😢</>}!
        <br /> Das Spiel war {resultPoints} Punkte wert!
        <br /> Damit bekommt {resultName} {resultWon ? resultPoints : -2*resultPoints} Punkte.
        <table>
            <tbody>
                {
                    Object.entries(state.scores).map(([name, score]) =>
                     <tr key={name}>
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