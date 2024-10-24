namespace KadiFs.Poker

module Utilities =
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

