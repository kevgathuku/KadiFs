// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

[<EntryPoint>]
let main argv =
    printfn "Starting game..."
    Poker.runStateMachine Poker.initialState