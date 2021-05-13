import { FinishedPhase, PublicInfo } from "./State"

export const Scoreboard: React.FC<{ phase: FinishedPhase, publicInfo: PublicInfo }> = ({ phase, publicInfo }) => {
  const { position, hasWon, gameValue, hasUeberreizt } = phase.scoringResult
  const sum = Object.values(phase.scores).reduce((x, y) => x + y, 0)
  const resultName = publicInfo.names[position] || position

  return <div>
    {resultName} hat {hasWon ? <>gewonnen</> :
      hasUeberreizt ? <>sich überreizt 😢😢😢</> : <>verloren 😢</>}!
        <br /> Das Spiel war {gameValue} Punkte wert!
        <br /> Damit bekommt {resultName} {hasWon ? gameValue : -2 * gameValue} Punkte.
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