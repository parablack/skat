import { ILobbyState } from "./State"

export const LobbyInput: React.FC<{ ws: WebSocket, state: ILobbyState }> = ({ ws, state }) => {
  return <div className="App" style={{
    height: "100%",
    overflow: 'hidden',
  }}>
    <section style={{
      display: 'flex',
      flexDirection: 'column',
      alignItems: 'center',
      justifyContent: 'center',
      fontSize: 'calc(10px + 2vmin)',
      color: 'white',
      height: '100%'
    }}>
      <div style={{
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        alignItems: 'center',
        height: '20%'
      }}>
        <header>Wilkommen auf der 🏝️Ramschinsel🏝️!</header>
      </div>

      <div style={{
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'normal',
        alignItems: 'center',
        height: '50%',
      }}>

        <p>
          <h3>Lobbies:</h3>
          <table style={{
            borderSpacing: "1em"
          }}>
            <thead>
              <tr>
                <th>Lobby</th>
                <th>Vorhand</th>
                <th>Mittelhand</th>
                <th>Geber</th>
                <th>Zuschauen</th>
              </tr>
            </thead>
            <tbody>
              {state.lobbies.map((lobby, index) => <tr key={index}>
                <td>{lobby.name || lobby.id}</td>
                {["Vorhand", "Mittelhand", "Geber", ":eyes:"].map(pos => <td key={pos}>
                  {lobby.names[pos] ? (
                    <span>{lobby.names[pos]}</span>
                  ) : (
                    pos === ':eyes:'
                      ? <button className="unicode-button" onClick={(_) => { ws.send(JSON.stringify({ action: "join-spectator-todo", })) }}>👀</button>
                      : <button onClick={() => {
                        ws.send(JSON.stringify({ action: "join", id: lobby.id, position: pos }))
                      }}>Mein Platz!</button>
                  )}
                </td>)}
              </tr>)}
            </tbody>
          </table>
        </p>
      </div>
    </section>
  </div>
}
