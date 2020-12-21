/// Count number of elements in array that satisfy filter
let countFilteredBy (f: ('a -> bool)) (array: 'a []): int =
    Array.fold (fun acc item -> if f item then acc + 1 else acc) 0 array

let input =
    System.IO.File.ReadAllLines("day10.txt")
    |> Array.map int

let outlet = 0
let device = ((Array.max input) + 3)

let all =
    [ outlet ]
    @ (input |> Array.toList)
    @ [ device ]
    |> List.toArray
    |> Array.sort

let diff =
    all
    |> Array.windowed 2
    |> Array.map (fun a -> a.[1] - a.[0])

let diff1 = countFilteredBy ((=) 1) diff
let diff3 = countFilteredBy ((=) 3) diff

printfn "Part 1: %d" (diff1 * diff3)
