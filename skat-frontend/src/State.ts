type Player = "Geber" | "Vorhand" | "Mittelhand"

export interface ICard { suit: string, name: string }

export type Stich = ([ICard, Player])[]

interface ICommonPlayerState {
    resign: number,
    you: IPlayerState,
    names: { [player: string]: string },
    cards: { [player: string]: ICard[] },
    turn: string,
    yourTurn: boolean
    singlePlayer?: string,
}

export interface IPlayerState {
    cards: ICard[],
    woncards: ICard[],
    position: Player
}

export interface IRunningState extends ICommonPlayerState {
    phase: "running",
    gamemode: { kind: "Ramsch" | "Null" | "Grand" } | {
        kind: "Farbspiel",
        color: string,
    },
    currentStich: Stich,
    lastStich: Stich
}

export interface IFinishedState extends ICommonPlayerState {
    phase: "finished",
    winner: string[],
    scores: { [player: string]: number },
    currentStich: Stich
}

export interface IReizState extends ICommonPlayerState {
    phase: "reizen",
    reizAnsagerTurn: boolean,
    reizCurrentBid: number,
    resign: number
}


export interface ISkatPickingPhase extends ICommonPlayerState {
    phase: "skatpicking",
    cardsToDiscard: ICard[]
}

export interface IGamePickingState extends ICommonPlayerState {
    phase: "gamepicking"
}


export interface IEmptyState extends ICommonPlayerState {
    phase: "empty"
}

export type IState = IFinishedState | IRunningState | IReizState | ISkatPickingPhase | IGamePickingState | IEmptyState

export const DEBUG_STATE: IState =
    { phase: "empty", names: {}, you: { "cards": [], "woncards": [], "position": "Vorhand" }, turn: "nobody", resign: 0, cards: { "Geber": [], "Vorhand": [], "Mittelhand": [] }, yourTurn: false }
    // { "yourTurn": false, "gameMode": { kind: "farbspiel", color: "Spades" }, "phase": "running", "currentStich": [], "lastStich": [], "you": { "cards": [], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand", "names": {}, "resign": 0, cards: { "Geber": [], "Vorhand": [], "Mittelhand": [] } }
    // { "yourTurn": true, "reizAnsagerTurn": true, "reizCurrentBid": 18, "phase": "reizen", "you": { "cards": [], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand", "names": {}, "resign": 0 }
