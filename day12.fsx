#load "Common.fsx"
open Common

type Action =
    | North
    | South
    | East
    | West
    | Forward
    | Left
    | Right

type Direction =
    | North
    | South
    | East
    | West

type State = Direction * (int * int)

let parse (command: string) =
    match command.[0] with
    | 'N' -> (Action.North, int command.[1..])
    | 'S' -> (Action.South, int command.[1..])
    | 'E' -> (Action.East, int command.[1..])
    | 'W' -> (Action.West, int command.[1..])
    | 'F' -> (Action.Forward, int command.[1..])
    | 'L' -> (Action.Left, int command.[1..])
    | 'R' -> (Action.Right, int command.[1..])
    | e -> failwith $"Unknown character: {e}"

let input =
    System.IO.File.ReadAllLines("day12.txt")
    |> Array.map parse

// Position in order (north, east)
let initial = State(Direction.East, (0, 0))

let forward state a =
    let (dir, (north, east)) = state
    match dir with
    | Direction.North -> State(dir, (north + a, east))
    | Direction.South -> State(dir, (north - a, east))
    | Direction.East -> State(dir, (north, east + a))
    | Direction.West -> State(dir, (north, east - a))

let rec turnLeft direction degrees =
    let turnLeft90 direction =
        match direction with
        | Direction.North -> Direction.West
        | Direction.West -> Direction.South
        | Direction.South -> Direction.East
        | Direction.East -> Direction.North

    if degrees % 360 = 0
    then direction
    else turnLeft (turnLeft90 direction) (degrees - 90)

let turnRight direction degrees = turnLeft direction (360 - degrees)

let move state action =
    let (dir, (north, east)) = state
    match action with
    | (Action.North, a) -> State(dir, (north + a, east))
    | (Action.South, a) -> State(dir, (north - a, east))
    | (Action.East, a) -> State(dir, (north, east + a))
    | (Action.West, a) -> State(dir, (north, east - a))
    | (Action.Forward, a) -> forward state a
    | (Action.Left, a) -> (turnLeft dir a, (north, east))
    | (Action.Right, a) -> (turnRight dir a, (north, east))

let manhattanTo0 pos =
    let (north, east) = pos
    (abs north) + (abs east)

#time
let part1 = input |> Array.fold move initial
let (dir, pos) = part1
printfn "Part 1: %d" (manhattanTo0 pos)


let rotateLeft waypoint angle =
    let (north, east) = waypoint
    match angle with
    | _ when angle % 360 = 0 -> waypoint
    | _ when angle % 360 = 90 -> (east, -north)
    | _ when angle % 360 = 180 -> (-north, -east)
    | _ when angle % 360 = 270 -> (-east, north)
    | _ -> failwith $"Unsupported angle {angle}"

type StateWithWaypoint =
    { Pos: (int * int)
      Waypoint: (int * int) }

let moveToWaypoint state factor =
    let (northPos, eastPos) = state.Pos
    let (north, east) = state.Waypoint
    (northPos + factor * north, eastPos + factor * east)

let initialWithWaypoint = { Pos = (0, 0); Waypoint = (1, 10) }

let moveWithWaypoint state action =
    let (north, east) = state.Waypoint
    // printfn "%A" state
    match action with
    | (Action.North, a) ->
        { state with
              Waypoint = (north + a, east) }
    | (Action.South, a) ->
        { state with
              Waypoint = (north - a, east) }
    | (Action.East, a) ->
        { state with
              Waypoint = (north, east + a) }
    | (Action.West, a) ->
        { state with
              Waypoint = (north, east - a) }
    | (Action.Forward, a) ->
        { state with
              Pos = (moveToWaypoint state a) }
    | (Action.Left, a) ->
        let waypoint = rotateLeft state.Waypoint (a % 360)
        { state with Waypoint = waypoint }
    | (Action.Right, a) ->
        let waypoint =
            rotateLeft state.Waypoint ((360 - a) % 360)

        { state with Waypoint = waypoint }

let example =
    [| "F10"; "N3"; "F7"; "R90"; "F11" |]
    |> Array.map parse

let part2 =
    input
    |> Array.fold moveWithWaypoint initialWithWaypoint

printfn "Part 2: %d" (manhattanTo0 part2.Pos)
