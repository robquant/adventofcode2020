open System.IO
open System.Text.RegularExpressions

let regex = Regex("(nw|ne|se|sw|e|w)")


let toMoves line =
    regex.Matches(line) |> Seq.map (fun m -> m.Value)

let isEven n = (n % 2 = 0)
let isOdd n = not (isEven n)

let move directions =
    let start = (0, 0)

    let step pos dir =
        let (c, r) = pos

        match dir with
        | "e" -> (c + 1, r)
        | "w" -> (c - 1, r)
        | "se" -> if isEven r then (c + 1, r + 1) else (c, r + 1)
        | "ne" -> if isEven r then (c + 1, r - 1) else (c, r - 1)
        | "sw" -> if isOdd r then (c - 1, r + 1) else (c, r + 1)
        | "nw" -> if isOdd r then (c - 1, r - 1) else (c, r - 1)
        | e -> failwith $"Unknown direction {e}"

    let (c, r) = directions |> Seq.fold step start
    (c, r)

let updateMap (blackTiles: Set<'a>) (field: 'a) =
    if Set.contains field blackTiles then Set.remove field blackTiles else Set.add field blackTiles

let blackTiles =
    File.ReadAllLines("day24.txt")
    |> Seq.map (toMoves >> move)
    |> Seq.fold updateMap Set.empty

printfn "Part 1: %d" (blackTiles.Count)

type TileState =
    | Black
    | White

let neighbours tile =
    let c, r = tile

    if isEven r then
        seq {
            (c + 1, r)
            (c - 1, r)
            (c + 1, r + 1)
            (c + 1, r - 1)
            (c, r + 1)
            (c, r - 1)
        }
    else
        seq {
            (c + 1, r)
            (c - 1, r)
            (c, r + 1)
            (c, r - 1)
            (c - 1, r + 1)
            (c - 1, r - 1)
        }


let calcNewState oldState blackNeighbours =
    match oldState with
    | Black when blackNeighbours = 0 || blackNeighbours > 2 -> White
    | White when blackNeighbours = 2 -> Black
    | Black -> Black
    | White -> White

let countActiveNeighbours (blackTiles: Set<'a>) (coords: 'a) (neighbours: 'a -> seq<'a>) =
    coords
    |> neighbours
    |> Seq.filter blackTiles.Contains
    |> Seq.length

let cycle (neighbours: 'a -> seq<'a>) (blackTiles: list<'a>) =
    let candidates =
        blackTiles
        |> Seq.collect neighbours
        |> Seq.distinct

    let blackTilesSet = Set.ofList blackTiles

    [ for candidate in candidates do
        let oldState =
            if blackTilesSet.Contains(candidate) then Black else White

        let activeNeighbours =
            countActiveNeighbours blackTilesSet candidate neighbours

        let newState = calcNewState oldState activeNeighbours
        if newState = Black then candidate ]

let repeat (count: int) (f: 'T -> 'T) (initial: 'T) =
    let rec helper i acc =
        if i < count then helper (i + 1) (f acc) else acc

    helper 0 initial

let part2 =
    repeat 100 (fun blackTiles -> cycle neighbours blackTiles) (List.ofSeq blackTiles)

printfn "Part 2: %d" (List.length part2)
