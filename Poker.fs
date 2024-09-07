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
    | AwaitingCards
    | Normal
    | Cardless
    | Kadi

type Player =
    { Cards: Deck
      Name: string
      State: PlayerState }

type GameAction =
    | Start of int
    | AddPlayer of string
    | AddDeck of Deck
    // Deal cards to players and deal start card
    | DealCards
    | Finish
    | Unknown

type PlayerAction =
    | PlayHand
    | Pick
    | Kadi
    | Finish

type GameStatus =
    | NotStarted
    | Lobby
    | AwaitingDeck
    | AwaitingAssignCards
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

let contains element list =
    List.exists (fun elem -> elem = element) list

let getStartCard (deck: Deck) (blocklist: CardValue list) =
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


printfn "For list %A, contains card 3D is %b" simpleDeck (contains threeDiamonds simpleDeck)
printfn "For list %A, contains card 7D is %b" simpleDeck (contains sevenDiamonds simpleDeck)

let suits = [ Hearts; Diamonds; Spades; Flowers ]
let values = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; J; Q; K; A ]

let cartesianProduct (suits: Suit list) (values: CardValue list) : Deck =
    let mergedLists =
        List.map (fun suit -> List.map (fun value -> { Suit = suit; Value = value }) values) suits

    List.concat mergedLists

let createDeck = cartesianProduct suits values

let mapReduce f initState list =
    let rec loop acc state =
        function
        | [] -> (List.rev acc, state) // Return reversed list and final state
        | x :: xs ->
            let (mappedValue, newState) = f x state
            loop (mappedValue :: acc) newState xs

    loop [] initState list

// let f x acc = (x * 2, acc + x)

// let result = mapReduce f 0 [1; 2; 3; 4]

// printfn "%A" result


let inputToGameAction (inputList) : GameAction =
    if List.length inputList > 2 then
        printfn "Too many commands!!"
        Unknown
    else
        match inputList with
        | head :: second :: _ ->
            match head, second with
            | "start", num ->
                printfn $"Creating game with {num} players..."
                Start(num |> int)
            | "add_player", name -> AddPlayer name
            | (_, _) ->
                printfn "Unknown command"
                Unknown
        | head :: _ ->
            match head with
            | "add_deck" -> AddDeck createDeck
            | "deal_cards" -> DealCards
            | other ->
                printfn $"Unknown command: {other}"
                Unknown
        | [] ->
            printfn "The command list is empty"
            Unknown

let transition action state =
    match state.Status, action with
    | NotStarted, Start(numPlayers) when numPlayers >= minPlayers && numPlayers <= maxPlayers ->
        { state with
            Status = Lobby
            NumPlayers = numPlayers }
    | NotStarted, Start(numPlayers) when numPlayers < minPlayers ->
        printfn "Not enough players. You need at least {maxPlayers} players. Please try again."
        state
    | NotStarted, Start(numPlayers) when numPlayers > maxPlayers ->
        printfn $"Too many players. Max allowed players is {maxPlayers}. Please try again."
        state
    | NotStarted, _ ->
        printfn $"Invalid action."
        state
    | Lobby, AddPlayer(playerName) ->
        // TODO: Add handling for existing player
        let updatedPlayers =
            { Name = playerName
              Cards = []
              State = AwaitingCards }
            :: state.Players

        let newStatus =
            if List.length (updatedPlayers) < state.NumPlayers then
                Lobby
            else
                AwaitingDeck

        { state with
            Status = newStatus
            Players = updatedPlayers }
    | Lobby, other ->
        printfn $"Unsupported action: {other}"
        state
    | AwaitingDeck, AddDeck(newDeck) ->
        let shuffledDeck = List.randomShuffle newDeck

        { state with
            PickDeck = shuffledDeck
            Status = AwaitingAssignCards }
    | AwaitingAssignCards, DealCards ->
        let assignPlayerCards player deck =
            let playerCards, remainingCards = List.splitAt 4 deck

            { player with
                Cards = playerCards
                State = Normal },
            remainingCards

        // Loop over players, assigning them cards, and updating the deck
        let updatedPlayers, updatedDeck =
            mapReduce assignPlayerCards state.PickDeck state.Players

        let startCard = getStartCard updatedDeck startBlocklist

        let remainingDeck =
            List.filter (fun card -> card.Suit <> startCard.Suit && card.Value <> startCard.Value) updatedDeck

        { state with
            PickDeck = remainingDeck
            PlayedStack = [startCard]
            Players = updatedPlayers
            Status = Live }
    | _ -> state


let rec runStateMachine state =
    match state.Status with
    | GameOver ->
        printfn "Game over. Exiting."
        0
    | _ ->
        // Print the current state
        printfn "Current state: %A" state

        // Automate a few of the initial commands
        let forwardedState =
            state
            |> (transition (Start 2))
            |> (transition (AddPlayer "kevin"))
            |> (transition (AddPlayer "another one"))
            |> (transition (AddDeck createDeck))
            |> (transition DealCards)

        printfn "Forwarded state: %A" forwardedState

        // Ask for user input
        printfn "Please enter the next command.."
        let input = System.Console.ReadLine()

        // Translate the input to an action
        let gameAction = inputToGameAction (input.Split([| ' ' |]) |> Array.toList)

        // Compute the next state based on input
        let nextState = transition gameAction forwardedState

        // Recursively run the state machine with the new state
        runStateMachine nextState
