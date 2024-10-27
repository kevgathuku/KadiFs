open KadiFs.Poker.Core
open KadiFs.Poker.Game
open KadiFs.Poker.Utilities


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
            | "play", cards ->
                let parsedCards = cards.Split([| ' ' |]) |> Seq.map parseCard
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


[<EntryPoint>]
let main argv =
    printfn "Starting game..."
    runStateMachine initialState
