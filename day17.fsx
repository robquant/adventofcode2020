let input =
    "day17.txt" |> System.IO.File.ReadAllLines

type CubeState =
    | Active
    | Inactive

let activeCubes3 =
    [ for (y, line) in input |> Seq.indexed do
        for (x, c) in line |> Seq.indexed -> (x, y, 0), c ]
    |> List.choose (fun (coords, symbol) ->
        match symbol with
        | '#' -> Some(coords)
        | _ -> None)

let activeCubes4 =
    [ for (y, line) in input |> Seq.indexed do
        for (x, c) in line |> Seq.indexed -> (x, y, 0, 0), c ]
    |> List.choose (fun (coords, symbol) ->
        match symbol with
        | '#' -> Some(coords)
        | _ -> None)

let neighbours3 coords =
    let delta = [| -1; 0; 1 |]
    let x, y, z = coords
    seq {
        for dx in delta do
            for dy in delta do
                for dz in delta do
                    if dx <> 0 || dy <> 0 || dz <> 0 then (x + dx, y + dy, z + dz)
    }

let neighbours4 coords =
    let delta = [| -1; 0; 1 |]
    let x, y, z, w = coords
    seq {
        for dx in delta do
            for dy in delta do
                for dz in delta do
                    for dw in delta do
                        if dx <> 0 || dy <> 0 || dz <> 0 || dw <> 0
                        then (x + dx, y + dy, z + dz, w + dw)
    }

let calcNewState oldState activeNeighbours =
    match oldState with
    | Active when activeNeighbours = 2 || activeNeighbours = 3 -> Active
    | Active -> Inactive
    | Inactive when activeNeighbours = 3 -> Active
    | Inactive -> Inactive

let countActiveNeighbours (activeCubes: Set<'a>) (coords: 'a) (neighbours: 'a -> seq<'a>) =
    coords
    |> neighbours
    |> Seq.filter activeCubes.Contains
    |> Seq.length

let cycle (neighbours: 'a -> seq<'a>) (activeCubes: list<'a>) =
    let candidates =
        activeCubes
        |> Seq.collect neighbours
        |> Seq.distinct

    let activeCubesSet = Set.ofList activeCubes
    [ for candidate in candidates do
        let oldState =
            if activeCubesSet.Contains(candidate) then Active else Inactive

        let activeNeighbours =
            countActiveNeighbours activeCubesSet candidate neighbours

        let newState = calcNewState oldState activeNeighbours
        if newState = Active then candidate ]

#time

let cycle3 activeCubes = cycle neighbours3 activeCubes
let cycle4 activeCubes = cycle neighbours4 activeCubes

let part1 =
    activeCubes3
    |> cycle3
    |> cycle3
    |> cycle3
    |> cycle3
    |> cycle3
    |> cycle3
    |> List.length

printfn "Part 1: %d" part1

let part2 =
    activeCubes4
    |> cycle4
    |> cycle4
    |> cycle4
    |> cycle4
    |> cycle4
    |> cycle4
    |> List.length

printfn "Part 2: %d" part2
