module LibraryTests

open Xunit
open KadiFs.Poker
open KadiFs.Poker.Core

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
    let lastPlayed = "3D" |> parseCard
    let hand = [ "5D"; "7D" ] |> List.map parseCard
    let result = Game.isValidSuitOrNumber lastPlayed hand

    Assert.Equal(true, result)

[<Fact>]
let ``Game.isValidSuitOrNumber invalid`` () =
    let lastPlayed = "3D" |> parseCard
    let hand = [ "5D"; "8H" ] |> List.map parseCard
    let result = Game.isValidSuitOrNumber lastPlayed hand

    Assert.Equal(false, result)

[<Fact>]
let ``Game.parseCard`` () =
    let result = parseCard "7H"
    let expected = { Value = Number 7; Suit = Hearts }

    Assert.Equal(expected, result)

[<Fact>]
let ``Game.containsQuestion`` () =
    let withEightHearts = [ "8H" ] |> List.map parseCard
    let withEightDiamonds = [ "8D" ] |> List.map parseCard
    let withEightFlowers = [ "8F" ] |> List.map parseCard
    let withEightSpades = [ "8S" ] |> List.map parseCard

    let withQueenHearts = [ "QH" ] |> List.map parseCard
    let withQueenDiamonds = [ "QD" ] |> List.map parseCard
    let withQueenFlowers = [ "QF" ] |> List.map parseCard
    let withQueenSpades = [ "QS" ] |> List.map parseCard

    let candidates =
        [ withEightHearts
          withEightDiamonds
          withEightFlowers
          withEightSpades
          withQueenHearts
          withQueenDiamonds
          withQueenFlowers
          withQueenSpades ]

    Assert.Equal(true, List.forall Game.containsQuestion candidates)
