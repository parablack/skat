type Player = "Geber" | "Vorhand" | "Mittelhand"

export interface ICard { suit: string, name: string }


export interface IPlayerState {
    cards: ICard[],
    woncards: ICard[],
    position: Player
}

export interface IState {
    you: IPlayerState,
    gamemode: "Ramsch",
    phase: "running",
    currentStich: ([ICard, Player])[],
    turn: string,
    yourTurn: boolean,
}
export const DEBUG_STATE: IState =
    { "yourTurn": false, "gamemode": "Ramsch", "phase": "running", "currentStich": [], "you": { "cards": [], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand" }