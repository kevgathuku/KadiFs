# Kadi

A collection of card games.

Starting off with Poker and the rest will be added with time.

### Prerequisites

- Install [.Net8](https://dotnet.microsoft.com/en-us/download)

#### Console

- To run the project, run `dotnet run --project DriverApp`
- For now the project is driven through the CLI, but a web interface is coming soon!

### Directory Structure
```
/KadiFs
   ├── /KadiFs.Poker
   │     ├── KadiFs.Poker.fsproj
   │     └── Library.fs
   ├── /DriverApp
   │     ├── DriverApp.fsproj
   │     └── Program.fs <--- Entrypoint
   └── MySolution.sln
```

Available commands roughly match the game actions.
```
start_game 2 -> start game with 2 players
add_player "mo salah" -> add player with provided name
add_deck -> add the deck (stil needed???)
deal_cards -> deal cards to the players
```

Support for individual player actions coming soon!
