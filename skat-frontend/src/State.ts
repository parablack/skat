type Player = "Geber" | "Vorhand" | "Mittelhand"

export interface ICard { suit: string, name: string }

export interface IScoring { hand: boolean, angesagt: string }

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
    lastStich: Stich,
    scoring: IScoring
}

export interface IFinishedState extends ICommonPlayerState {
    phase: "finished",
    // player, won, game value, überreizt?
    result: [string, boolean, number, boolean],
    scores: { [player: string]: number },
    currentStich: Stich
}

export interface IReizState extends ICommonPlayerState {
    phase: "reizen",
    reizAnsagerTurn: boolean,
    reizCurrentBid: number,
    resign: number
}

export interface IHandPickingPhase extends ICommonPlayerState {
    phase: "handpicking",
}

export interface ISkatPickingPhase extends ICommonPlayerState {
    phase: "skatpicking",
    cardsToDiscard: ICard[]
}

export interface IGamePickingState extends ICommonPlayerState {
    phase: "gamepicking"
}


export interface IEmptyState {
    phase: "empty"
}

export interface ILobby {
    id: number,
    name?: string,
    names: { [player: string]: string },
}

export interface ILobbyState {
    phase: "lobby",
    lobbies: ILobby[],
}

export type IGameState = IReizState
    | IHandPickingPhase
    | ISkatPickingPhase
    | IGamePickingState
    | IRunningState
    | IFinishedState

export type IState = IEmptyState
    | ILobbyState
    | IReizState
    | IHandPickingPhase
    | ISkatPickingPhase
    | IGamePickingState
    | IRunningState
    | IFinishedState

export function inLobby(state: IState): state is IGameState {
    return (
        state.phase === 'reizen' ||
        state.phase === 'handpicking' ||
        state.phase === 'skatpicking' ||
        state.phase === 'gamepicking' ||
        state.phase === 'running' ||
        state.phase === 'finished'
    )
}

export const EMPTY_STATE: IState = { phase: "empty" }
    // {phase: 'lobby', lobbies: [ { id: 0, name: 'Die Erste', names: { Vorhand: 'Mflo3000' } } ]}
    // { "yourTurn": false, "gameMode": { kind: "farbspiel", color: "Spades" }, "phase": "running", "currentStich": [], "lastStich": [], "you": { "cards": [], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand", "names": {}, "resign": 0, cards: { "Geber": [], "Vorhand": [], "Mittelhand": [] } }
    // { "yourTurn": true, "reizAnsagerTurn": true, "reizCurrentBid": 18, "phase": "reizen", "you": { "cards": [], "woncards": [], "position": "Vorhand" }, "turn": "Vorhand", "names": {}, "resign": 0 }
