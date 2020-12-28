open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

let regex = Regex("(nw|ne|se|sw|e|w)")


let toMoves line =
    regex.Matches(line) |> Seq.map (fun m -> m.Value)

let isEven n = (n % 2 = 0)
let isOdd n = not (isEven n)

let move directions =
    let start = struct (0, 0)

    let step pos dir =
        let struct (c, r) = pos

        match dir with
        | "e" -> struct (c + 1, r)
        | "w" -> struct (c - 1, r)
        | "se" -> if isEven r then struct (c + 1, r + 1) else struct (c, r + 1)
        | "ne" -> if isEven r then struct (c + 1, r - 1) else struct (c, r - 1)
        | "sw" -> if isOdd r then struct (c - 1, r + 1) else struct (c, r + 1)
        | "nw" -> if isOdd r then struct (c - 1, r - 1) else struct (c, r - 1)
        | e -> failwith $"Unknown direction {e}"

    let struct (c, r) = directions |> Seq.fold step start
    struct (c, r)

let updateMap (blackTiles: Set<'a>) (field: 'a) =
    if Set.contains field blackTiles then Set.remove field blackTiles else Set.add field blackTiles


type TileState =
    | Black
    | White

let neighbours tile =
    let struct (c, r) = tile

    if isEven r then
        [| struct (c + 1, r)
           struct (c - 1, r)
           struct (c + 1, r + 1)
           struct (c + 1, r - 1)
           struct (c, r + 1)
           struct (c, r - 1) |]
    else
        [| struct (c + 1, r)
           struct (c - 1, r)
           struct (c, r + 1)
           struct (c, r - 1)
           struct (c - 1, r + 1)
           struct (c - 1, r - 1) |]


let calcNewState oldState blackNeighbours =
    match oldState with
    | Black when blackNeighbours = 0 || blackNeighbours > 2 -> White
    | White when blackNeighbours = 2 -> Black
    | oldState -> oldState

let countActiveNeighbours (blackTiles: HashSet<'a>) (coords: 'a) (neighbours: 'a -> array<'a>) =
    coords
    |> neighbours
    |> Array.filter blackTiles.Contains
    |> Array.length

let cycle (neighbours: 'a -> array<'a>) (blackTiles: HashSet<'a>) =
    let candidates =
        blackTiles |> Seq.collect neighbours |> HashSet

    seq {
        for candidate in candidates do
            let oldState =
                if blackTiles.Contains(candidate) then Black else White

            let activeNeighbours =
                countActiveNeighbours blackTiles candidate neighbours

            let newState = calcNewState oldState activeNeighbours
            if newState = Black then candidate
    }
    |> HashSet

let repeat (count: int) (f: 'T -> 'T) (initial: 'T) =
    let rec helper i acc =
        if i < count then helper (i + 1) (f acc) else acc

    helper 0 initial

#time

let blackTiles =
    File.ReadAllLines("day24.txt")
    |> Seq.map (toMoves >> move)
    |> Seq.fold updateMap Set.empty

printfn "Part 1: %d" (blackTiles.Count)

let part2 =
    repeat 100 (fun blackTiles -> cycle neighbours blackTiles) (blackTiles |> HashSet)

printfn "Part 2: %d" (Seq.length part2)
