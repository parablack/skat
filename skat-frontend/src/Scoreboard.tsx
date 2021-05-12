import { FinishedPhase, PublicInfo } from "./State"

export const Scoreboard: React.FC<{ phase: FinishedPhase, publicInfo: PublicInfo }> = ({ phase, publicInfo }) => {

  let sum = Object.values(phase.scores).reduce((x, y) => x + y, 0)

  let resultName = publicInfo.names[phase.result[0]] || phase.result[0]
  let resultWon = phase.result[1];
  let resultPoints = phase.result[2];
  let resultUeberreizt = phase.result[3];

  return <div>
    {resultName} hat {resultWon ? <>gewonnen</> :
      resultUeberreizt ? <>sich überreizt 😢😢😢</> : <>verloren 😢</>}!
        <br /> Das Spiel war {resultPoints} Punkte wert!
        <br /> Damit bekommt {resultName} {resultWon ? resultPoints : -2 * resultPoints} Punkte.
        <table>
      <tbody>
        {Object.entries(phase.scores).map(([name, score]) =>
          <tr key={name}>
            <td>{publicInfo.names[name] || name}</td>
            <td>{score}</td>
          </tr>)}
        <tr>
          <td><i>Im Skat übrig</i></td>
          <td><i>{120 - sum}</i></td>
        </tr>
      </tbody>
    </table>
  </div>
}