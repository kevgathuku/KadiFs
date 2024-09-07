module Poker

type Suit =
    | Hearts
    | Diamonds
    | Spades
    | Flowers

type CardValue =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | J
    | Q
    | K
    | A

type Card = { Suit: Suit; Value: CardValue }

type Deck = Card list

type PlayerState =
    | Normal
    | Kadi

type Player =
    { cards: Deck
      name: string
      state: PlayerState }

type GameAction =
    | Start of int
    | AddPlayer of string
    | AddDeck of Deck
    // combine deal player cards and deal start cards into one action
    | DealCard of Deck
    | Finish

type PlayerAction =
    | PlayHand
    | Pick
    | Kadi
    | Finish

type GameStatus =
    | NotStarted
    | Lobby
    | AwaitingDeck
    | AwaitingPlayerCards
    | AwaitingStartCard
    | Live
    | Kadi
    | GameOver

type GameState =
    { Status: GameStatus
      NumPlayers: int
      Players: Player list
      PickDeck: Deck
      PlayedStack: Deck
      PlayerTurn: int }

// Game constants
let minPlayers = 2
let maxPlayers = 5
let cardsToDeal = 4

let startBlocklist = [ K; Q; J; A; Two; Three; Eight ]

// TODO: Can this be computed by removing 'A' from the start_cards_blocklist??
let finishBlocklist = [ K; Q; J; Two; Three; Eight ]

let initialState =
    { Status = GameStatus.NotStarted
      NumPlayers = 0
      Players = []
      PickDeck = []
      PlayedStack = []
      PlayerTurn = 0 }

printfn "Initial State: %A" initialState

let contains element list =
    List.exists (fun elem -> elem = element) list

let getFirstAllowedCard (deck: Deck) (blocklist: CardValue list) =
    List.find (fun card -> not (contains card.Value blocklist)) deck

let threeDiamonds =
    { Suit = Suit.Diamonds
      Value = CardValue.Three }

let fiveDiamonds =
    { Suit = Suit.Diamonds
      Value = CardValue.Five }

let sevenDiamonds =
    { Suit = Suit.Diamonds
      Value = CardValue.Seven }

let simpleDeck = [ threeDiamonds; fiveDiamonds ]

printfn "Initial State: %A" initialState
printfn "For list %A, contains card 3D is %b" simpleDeck (contains threeDiamonds simpleDeck)
printfn "For list %A, contains card 7D is %b" simpleDeck (contains sevenDiamonds simpleDeck)

let suits = [ Hearts; Diamonds; Spades; Flowers ]
let values = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; J; Q; K; A ]

let cartesianProduct (suits: Suit list) (values: CardValue list) : Deck =
    let mergedLists =
        List.map (fun suit -> List.map (fun value -> { Suit = suit; Value = value }) values) suits

    List.concat mergedLists

let createDeck = cartesianProduct suits values
