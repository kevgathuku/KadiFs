namespace KadiFs.Poker

open KadiFs.Poker.Core

module Game =

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
        | EnforcePick of int // Playing a card that forces the next player to pick
        | AcceptPick
        | NoCardsPick // Player has no cards. Pick and go to next player
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

    let isValidSuitOrNumber lastPlayed hand =
        (lastPlayed :: hand)
        |> Utilities.adjacentPairs
        |> List.forall (fun (lastCard, currentCard) -> Utilities.isSameSuitOrNumber lastCard currentCard)

    let containsQuestion hand =
        List.exists (fun card -> card.Value = Number 8 || card.Value = Queen) hand


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
        | Live, ProcessPlayerAction(NoCardsPick) ->
            // Assign a card to the current player from the deck
            let cardToAssign = List.head state.PickDeck
            let currentPlayer = state.Players[state.PlayerTurn]
            let updatedPlayerCards = cardToAssign :: currentPlayer.Cards
            let updatedCurrentPlayer = {currentPlayer with Cards = updatedPlayerCards}

            // Move on to the next player
            let updatedPlayers =
                List.map
                    (fun player ->
                        if player.Name = currentPlayer.Name then
                            updatedCurrentPlayer
                        else
                            player)
                    state.Players

            let remainingDeck = List.tail state.PickDeck
            { state with
                PickDeck = remainingDeck
                PlayerTurn = (state.PlayerTurn + 1) % state.NumPlayers
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
