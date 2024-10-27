module CoreTests

open Xunit
open KadiFs.Poker.Core

[<Fact>]
let ``Core.parseCard`` () =
    let expected = { Suit = Hearts; Value = Jack }
    
    let shorthand = parseCard "JH"
    let shorthandUnicode = parseCard "J♥"

    Assert.Equal(expected, shorthand)
    Assert.Equal(expected, shorthandUnicode)
