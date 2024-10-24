module LibraryTests

open Xunit
open KadiFs.Poker
open KadiFs.Poker.Game

[<Fact>]
let ``Game.minPlayers`` () =
    let expected = 2
    let result = Game.minPlayers

    Assert.Equal(expected, result)

[<Fact>]
let ``Game.finishBlocklist`` () =
    let expected = [ King; Queen; Jack; Number 2; Number 3; Number 8 ]
    let result = Game.finishBlocklist

    Assert.Equal<CardValue list>(expected, result)

[<Fact>]
let ``Game.isValidSuitOrNumber`` () =
    let lastPlayed = threeDiamonds
    let hand = [ fiveDiamonds; sevenDiamonds ]
    let result = isValidSuitOrNumber lastPlayed hand

    Assert.Equal(true, result)

[<Fact>]
let ``Game.isValidSuitOrNumber invalid`` () =
    let lastPlayed = threeDiamonds
    let hand = [ fiveDiamonds; { Value = Number 8; Suit = Hearts} ]
    let result = isValidSuitOrNumber lastPlayed hand

    Assert.Equal(false, result)

[<Fact>]
let ``Game.parseCard`` () =
    let result = parseCard "7H"
    let expected = { Value = Number 7; Suit = Hearts }

    Assert.Equal(expected, result)
