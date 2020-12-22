open System
open System.IO

let inputs =
    File.ReadAllText("day22.txt").Split("\n\n")

let parse (input: string array) =
    input.[1..] |> Seq.map int |> List.ofSeq

let deck1 =
    parse (
        inputs.[0]
            .Split("\n", StringSplitOptions.RemoveEmptyEntries)
    )

let deck2 =
    parse (
        inputs.[1]
            .Split("\n", StringSplitOptions.RemoveEmptyEntries)
    )

let rec game1 (deck1: int list) (deck2: int list) history =

    if Set.contains (deck1, deck2) history then
        (1, deck1)
    else
        match (deck1, deck2) with
        | (x, []) -> (1, x)
        | ([], x) -> (2, x)
        | (x1 :: x1s, x2 :: x2s) ->
            if x1 > x2
            then game1 (x1s @ [ x1; x2 ]) x2s (Set.add (deck1, deck2) history)
            else game1 x1s (x2s @ [ x2; x1 ]) (Set.add (deck1, deck2) history)


let rec game2 (deck1: int list) (deck2: int list) history =

    if Set.contains (deck1, deck2) history then
        (1, deck1)
    else
        match (deck1, deck2) with
        | (x, []) -> (1, x)
        | ([], x) -> (2, x)
        | (x1 :: x1s, x2 :: x2s) ->

            let winner =
                if (x1s.Length >= x1) && (x2s.Length >= x2)
                then fst (game2 (List.take x1 x1s) (List.take x2 x2s) Set.empty)
                else if x1 > x2
                then 1
                else 2

            if winner = 1

            then
                let deck1new = x1s @ [ x1; x2 ]
                game2 deck1new x2s (Set.add (deck1, deck2) history)
            else
                let deck2new = x2s @ [ x2; x1 ]
                game2 x1s deck2new (Set.add (deck1, deck2) history)

let add el (index, sum) = (index + 1, sum + index * el)
let value deck = snd (List.foldBack add deck (1, 0))

#time
let (_, windeck1) = game1 deck1 deck2 Set.empty
let (_, windeck2) = game2 deck1 deck2 Set.empty

printfn "Part 1: %d" (value windeck1)
printfn "Part 2: %d" (value windeck2)
