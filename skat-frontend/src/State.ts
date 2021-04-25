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
    resign: number,
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
    resign: number,
    you: IPlayerState,
    names: {[player: string]:string},
    winner: string,
    scores: {[player: string]:number},
    currentStich: Stich
}
export interface IReizState {
    phase: "reizen",
    you: IPlayerState,
    turn: string,
    names: {[player: string]:string},
    yourTurn: boolean,
    reizAnsagerTurn: boolean,
    reizCurrentBid: number,
    resign: number
}

type IState = IFinishedState | IRunningState | IReizState

export const DEBUG_STATE: IState =
   // { "yourTurn": false, "gamemode": "Ramsch", "phase": "running", "currentStich": [], "lastStich": [], "you": { "cards": [], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand", "names": {}, "resign": 0 }
   { "yourTurn": true, "reizAnsagerTurn": true, "reizCurrentBid": 18, "phase": "reizen", "you": { "cards": [], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand", "names": {}, "resign": 0 }
