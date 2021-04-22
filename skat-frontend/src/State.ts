type Player = "Geber" | "Vorhand" | "Mittelhand"

export interface ICard { suit: string, name: string }

export type Stich = ([ICard, Player])[]

export interface IPlayerState {
    cards: ICard[],
    woncards: ICard[],
    position: Player
}

export interface IRunningState {
    phase: "running",
    you: IPlayerState,
    gamemode: string,
    currentStich: Stich,
    lastStich: Stich,
    turn: string,
    yourTurn: boolean,
    names: {[player: string]:string},
}
export interface IFinishedState {
    phase: "finished",
    you: IPlayerState,
    names: {[player: string]:string},
    winner: string,
    scores: {[player: string]:number},
    currentStich: Stich
}

type IState = IFinishedState | IRunningState

export const DEBUG_STATE: IState =
   { "yourTurn": false, "gamemode": "Ramsch", "phase": "running", "currentStich": [], "lastStich": [], "you": { "cards": [], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand", "names": {} }