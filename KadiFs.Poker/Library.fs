namespace KadiFs.Poker

module Game =
    type Suit =
        | Hearts
        | Diamonds
        | Spades
        | Flowers

    let parseSuit =
        function
        | 'H' -> Hearts
        | 'S' -> Spades
        | 'D' -> Diamonds
        | 'C' -> Flowers
        | _ -> failwith "Invalid suit"

    type CardValue =
        | Number of int
        | Jack
        | Queen
        | King
        | Ace

    let parseValue =
        function
        | "A" -> Ace
        | "K" -> King
        | "Q" -> Queen
        | "J" -> Jack
        | "10" -> Number 10
        | v when v.Length = 1 && System.Char.IsDigit(v.[0]) -> Number(int v)
        | _ -> failwith "Invalid card value"

    [<StructuralComparison; StructuralEquality>]
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

    type PlayerAction =
        | PlayHand of Card list
        | AcceptPick
        | NoCardsPick // which is this one??
        | Jump
        | Kickback
        | Kadi
        | Finish

    type GameAction =
        | Start of int
        | AddPlayer of string
        | AddDeck of Deck
        // Deal cards to players and deal start card
        | DealCards
        | ProcessPlayerAction of PlayerAction
        | Finish
        | Unknown

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

    let startBlocklist = [ King; Queen; Jack; Ace; Number 2; Number 3; Number 8 ]

    // startBlocklist excluding 'A'
    let finishBlocklist = List.where (fun elm -> not (elm = Ace)) startBlocklist

    let initialState =
        { Status = NotStarted
          NumPlayers = 0
          Players = []
          PickDeck = []
          PlayedStack = []
          PlayerTurn = 0 }

    let getStartCard (deck: Deck) (blocklist: CardValue list) =
        List.find (fun card -> not (Utilities.contains card.Value blocklist)) deck

    let suits = [ Hearts; Diamonds; Spades; Flowers ]

    let values =
        [ Number 2
          Number 3
          Number 4
          Number 5
          Number 6
          Number 7
          Number 8
          Number 9
          Number 10
          Jack
          Queen
          King
          Ace ]

    let parseCard (shorthand: string) =
        let valuePart = shorthand.Substring(0, shorthand.Length - 1)
        let suitPart = shorthand.[shorthand.Length - 1]

        { Suit = parseSuit suitPart
          Value = parseValue valuePart }

    let cartesianProduct (suits: Suit list) (values: CardValue list) : Deck =
        let mergedLists =
            List.map (fun suit -> List.map (fun value -> { Suit = suit; Value = value }) values) suits

        List.concat mergedLists

    let createDeck = cartesianProduct suits values

    let isSameSuitOrNumber first second =
        first.Suit = second.Suit || first.Value = second.Value

    let isValidSuitOrNumber lastPlayed hand =
        (lastPlayed :: hand)
        |> Utilities.adjacentPairs
        |> List.forall (fun (lastCard, currentCard) -> isSameSuitOrNumber lastCard currentCard)

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
                | "play", lst ->
                    let parsedCards = lst.Split([| ' ' |]) |> Seq.map parseCard
                    ProcessPlayerAction(PlayHand(Seq.toList parsedCards))
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
                Utilities.mapReduce assignPlayerCards state.PickDeck state.Players

            let startCard = getStartCard updatedDeck startBlocklist

            let remainingDeck =
                List.filter (fun card -> card.Suit <> startCard.Suit && card.Value <> startCard.Value) updatedDeck

            { state with
                PickDeck = remainingDeck
                PlayedStack = [ startCard ]
                Players = updatedPlayers
                Status = Live }
        | Live, ProcessPlayerAction(PlayHand hand) ->
            // Ensure the cards come from the correct player i.e. the current turn
            let currentPlayer = state.Players[state.PlayerTurn]
            let lastPlayedCard = List.head state.PlayedStack

            let validPlayerCards hand =
                List.forall (fun card -> Utilities.contains card currentPlayer.Cards) hand

            let isAllowed = validPlayerCards hand && isValidSuitOrNumber lastPlayedCard hand
            // Add the rest of the checks
            //  - Valid cards to begin with -> number, ordering
            if isAllowed then
                // Add the cards to the played stack and remove them from player cards
                let currentPlayerCards = Utilities.removeItems currentPlayer.Cards hand

                let updatedCurrentPlayer =
                    { currentPlayer with
                        Cards = currentPlayerCards }

                let updatedPlayers =
                    List.map
                        (fun player ->
                            if player.Name = currentPlayer.Name then
                                updatedCurrentPlayer
                            else
                                player)
                        state.Players

                let newStack = List.rev (hand) @ state.PlayedStack

                { state with
                    PlayedStack = newStack
                    PlayerTurn = (state.PlayerTurn + 1) % List.length state.Players
                    Players = updatedPlayers }
            else
                state
        // ...
        // Proceed to the next player

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
