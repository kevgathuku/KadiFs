module UtilitiesTests

open Xunit
open KadiFs.Poker
open KadiFs.Poker.Core

let threeDiamonds = parseCard "3D"
let fiveDiamonds = parseCard "5D"
let sevenDiamonds = parseCard "7D"

[<Fact>]
let ``Utilities.contains`` () =
    let simpleDeck = [ "3D"; "5D" ] |> List.map parseCard

    Assert.Equal(true, Utilities.contains threeDiamonds simpleDeck)
    Assert.Equal(true, Utilities.contains fiveDiamonds simpleDeck)
    Assert.Equal(false, Utilities.contains sevenDiamonds simpleDeck)
