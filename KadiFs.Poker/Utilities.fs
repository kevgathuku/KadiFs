namespace KadiFs.Poker

open KadiFs.Poker.Core

module Utilities =
    let suits = [ Hearts; Diamonds; Spades; Flowers ]

    let values =
        [ Number 2
          Number 3
          Number 4
          Number 5
          Number 6
          Number 7
          Number 8
          Number 9
          Number 10
          Jack
          Queen
          King
          Ace ]

    let cartesianProduct (suits: Suit list) (values: CardValue list) : Deck =
        let mergedLists =
            List.map (fun suit -> List.map (fun value -> { Suit = suit; Value = value }) values) suits

        List.concat mergedLists

    let createDeck = cartesianProduct suits values

    let isSameSuitOrNumber first second =
        first.Suit = second.Suit || first.Value = second.Value

    let mapReduce f initState list =
        let rec loop acc state =
            function
            | [] -> (List.rev acc, state) // Return reversed list and final state
            | x :: xs ->
                let (mappedValue, newState) = f x state
                loop (mappedValue :: acc) newState xs

        loop [] initState list

    let contains element list =
        List.exists (fun elem -> elem = element) list

    let removeItems originalList elementsToRemove =
        originalList |> List.filter (fun x -> not (List.contains x elementsToRemove))

    let rec adjacentPairs lst =
        match lst with
        | a :: b :: rest -> (a, b) :: adjacentPairs (b :: rest)
        | _ -> []
