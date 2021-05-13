export type Player = "Geber" | "Vorhand" | "Mittelhand"

export interface Card { suit: string, name: string }

export interface ScoringInfo { hand: boolean, angesagt: string }

export type Stich = ([Card, Player])[]

export type GameMode = { kind: "Ramsch" | "Null" | "Grand" } | {
  kind: "Farbspiel",
  color: string,
};

export interface ReizPhase {
  phase: "ReizPhase",
  reizTurn: Player,
  reizBid: number,
}

export interface PickingPhase {
  phase: "PickingPhase",
  subPhase: "PickingHand" | "DiscardingSkat" | "PickingGamemode",
  pickingPlayer: Player,
  cardsToDiscard: number,
  isPlayingHand: boolean,
}

export interface RunningPhase {
  phase: "RunningPhase",
  gameMode: GameMode,
  scoring: ScoringInfo,
  currentStich: Stich,
  lastStich: Stich,
  singlePlayer: Player | null
}

export interface FinishedPhase {
  phase: "FinishedPhase",
  // player, won, game value, überreizt?
  result: [string, boolean, number, boolean],
  scores: { [player: string]: number },
  currentStich: Stich
}

export interface PublicInfo {
  turn: Player | null,
  cards: { [player: string]: Card[] },
  names: { [player: string]: string },
  numResigned: number,
}
export interface PrivateInfo {
  yourPosition: Player,
  yourTurn: boolean,
  yourCards: Card[],
  wonCards: Card[],
  showingCards: boolean,
  resigned: boolean,
}
export type Phase = ReizPhase | PickingPhase | RunningPhase | FinishedPhase


export interface EmptyState {
  type: "empty",
}

export interface PlayerState {
  type: "playerState",
  phase: Phase,
  public: PublicInfo,
  private: PrivateInfo,
}

export interface SpectatorState {
  type: "spectatorState",
  phase: Phase,
  public: PublicInfo,
  private: PrivateInfo,
}

export interface Lobby {
  id: number,
  name?: string,
  names: { [player: string]: string },
}


export interface LobbyState {
  type: "lobby",
  lobbies: Lobby[],
}

export type State = EmptyState | LobbyState | PlayerState | SpectatorState

export const EMPTY_STATE: State = { type: "empty" }
