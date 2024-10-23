module LibraryTests

open Xunit
open KadiFs.Poker

[<Fact>]
let ``Game.minPlayers`` () =
    let expected = 2
    let result = Game.minPlayers

    Assert.Equal(expected, result)