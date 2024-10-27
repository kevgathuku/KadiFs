namespace KadiFs.Poker

module Core =
    type Suit =
        | Hearts
        | Diamonds
        | Spades
        | Flowers

    let parseSuit =
        function
        | 'H' -> Hearts
        | 'S' -> Spades
        | 'D' -> Diamonds
        | 'F' -> Flowers
        | _ -> failwith "Invalid suit"

    type CardValue =
        | Number of int
        | Jack
        | Queen
        | King
        | Ace

    let parseValue =
        function
        | "A" -> Ace
        | "K" -> King
        | "Q" -> Queen
        | "J" -> Jack
        | "10" -> Number 10
        | v when v.Length = 1 && System.Char.IsDigit(v.[0]) -> Number(int v)
        | _ -> failwith "Invalid card value"

    [<StructuralComparison; StructuralEquality>]
    type Card = { Suit: Suit; Value: CardValue }

    type Deck = Card list

    let parseCard (shorthand: string) =
        let valuePart = shorthand.Substring(0, shorthand.Length - 1)
        let suitPart = shorthand.[shorthand.Length - 1]

        { Suit = parseSuit suitPart
          Value = parseValue valuePart }
