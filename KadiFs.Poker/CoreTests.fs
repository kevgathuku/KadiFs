module CoreTests

open Xunit
open KadiFs.Poker.Core

[<Fact>]
let ``Core.parseCard`` () =
    let expected = { Suit = Hearts; Value = Jack }
    let result = parseCard "JH"

    Assert.Equal(expected, result)
