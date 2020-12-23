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

type Hash =
    struct
        val hash1: int
        val hash2: int
        new(h1: int, h2: int) = { hash1 = h1; hash2 = h2 }
    end

let rec game2 (deck1: int array) (deck2: int array) (history: ResizeArray<Hash>) =

    if history.Contains(Hash(hash deck1, hash deck2)) then
        (1, deck1)
    else

    if Array.isEmpty deck2 then
        (1, deck1)
    elif Array.isEmpty deck1 then
        (2, deck2)
    else
        let x1 = Array.head deck1
        let x2 = Array.head deck2

        let winner =
            if (Array.length deck1 > x1)
               && (Array.length deck2 > x2) then
                fst (game2 (Array.sub deck1 1 x1) (Array.sub deck2 1 x2) (ResizeArray<Hash>()))
            else if x1 > x2 then
                1
            else
                2

        let x1s = Array.tail deck1
        let x2s = Array.tail deck2

        history.Add(Hash(hash deck1, hash deck2))

        if winner = 1

        then
            let deck1new = Array.append x1s [| x1; x2 |]
            game2 deck1new x2s history
        else
            let deck2new = Array.append x2s [| x2; x1 |]
            game2 x1s deck2new history

let add el (index, sum) = (index + 1, sum + index * el)
let value deck = snd (List.foldBack add deck (1, 0))

#time
let (_, windeck1) = game1 deck1 deck2 Set.empty

let (_, windeck2) =
    game2 (deck1 |> Array.ofList) (deck2 |> Array.ofList) (ResizeArray<Hash>())

printfn "Part 1: %d" (value windeck1)
printfn "Part 2: %d" (value (windeck2 |> List.ofArray))
