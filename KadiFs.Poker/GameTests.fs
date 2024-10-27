module GameTests

open Xunit
open KadiFs.Poker
open KadiFs.Poker.Core
open KadiFs.Poker.Game

[<Fact>]
let ``Game.minPlayers`` () =
    let expected = 2
    let result = minPlayers

    Assert.Equal(expected, result)

[<Fact>]
let ``Game.finishBlocklist`` () =
    let expected = [ King; Queen; Jack; Number 2; Number 3; Number 8 ]
    let result = finishBlocklist

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
    let result = isValidSuitOrNumber lastPlayed hand

    Assert.Equal(false, result)

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

    Assert.Equal(true, List.forall containsQuestion candidates)

[<Fact>]
let ``Game.isValidQuestionAnswer`` () =
    let lastPlayedCard = parseCard "6♥"

    let checkHand = isValidQuestionAnswer lastPlayedCard

    Assert.Equal(true, checkHand (parseCards [ "8♥"; "2♥" ]))
    Assert.Equal(true, checkHand (parseCards [ "8♥"; "Q♥"; "4♥"; "4♦" ]))

    Assert.Equal(false, checkHand (parseCards [ "8♥"; "Q♥" ]))
    Assert.Equal(false, checkHand (parseCards [ "Q♥"; "Q♦" ]))
    Assert.Equal(false, checkHand (parseCards [ "7♥" ])) // No question
    Assert.Equal(false, checkHand (parseCards [ "7♦" ])) // Non-matching suits


[<Fact>]
let ``Game.ProcessPlayerAction NoCardsPick`` () =
    let pickDeck = [ "A♠"; "4♠"; "3♦"; "2♣" ] |> List.map parseCard

    let playerOne: Player =
        { Cards = [ "J♥"; "K♠"; "9♣"; "8♣" ] |> List.map parseCard
          Name = "king"
          State = Normal }

    let playerTwo: Player =
        { Cards = [ "3♣"; "10♥"; "K♥"; "7♦" ] |> List.map parseCard
          Name = "kaka"
          State = Normal }

    let gameState =
        { initialState with
            Status = Live
            NumPlayers = 2
            PickDeck = pickDeck
            PlayerTurn = 0
            PlayedStack = [ "7♥" ] |> List.map parseCard
            Players = [ playerOne; playerTwo ] }

    let updatedPlayerOne =
        { playerOne with
            Cards = (List.head pickDeck :: playerOne.Cards) }

    let action = ProcessPlayerAction(NoCardsPick)

    let expected =
        { gameState with
            Players = [ updatedPlayerOne; playerTwo ]
            PlayerTurn = 1
            PickDeck = List.tail pickDeck }

    let result = transition action gameState

    Assert.Equal(expected, result)
