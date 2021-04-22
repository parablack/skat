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
    { "yourTurn": true, "gamemode": "Ramsch", "phase": "running", "currentStich": [], "you": { "cards": [{ "suit": "Hearts", "name": "Nine" }, { "suit": "Spades", "name": "Nine" }, { "suit": "Clubs", "name": "Ten" }, { "suit": "Diamonds", "name": "Ten" }, { "suit": "Hearts", "name": "Ten" }, { "suit": "Spades", "name": "Ten" }, { "suit": "Clubs", "name": "Jack" }, { "suit": "Diamonds", "name": "Jack" }, { "suit": "Hearts", "name": "Jack" }, { "suit": "Spades", "name": "Jack" }], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand" }