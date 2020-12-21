#load "Common.fsx"

open Common

let input = System.IO.File.ReadAllLines("day13.txt")

let earliestDepart = int input.[0]

let cadences =
    input.[1].Split(",")
    |> Array.filter (fun e -> e <> "x")
    |> Array.map int

let nextDeparture =
    cadences
    |> Array.map (fun bus -> (((earliestDepart / bus) + 1) * bus, bus))
    |> Array.minBy (fst)

let (nextBusTime, nextBus) = nextDeparture
printfn "Part 1: %d" ((nextBusTime - earliestDepart) * nextBus)

// let test = "1789,37,47,1889"
// let test = "17,x,13,19"
// let test = "67,7,59,61"
let test = "67,7,x,59,61"

/// Euclidean remainder, the proper modulo operation
let inline (%!) a b = (a % b + b) % b

let toRemainder (i, e) =
    let n = int64 e
    (uint64 n, uint64 ((n - int64 i) %! n))

let remainders =
    input.[1].Split(",")
    // test.Split(",")
    |> Array.mapi (fun i e -> (i, e))
    |> Array.filter (fun (i, e) -> e <> "x")
    |> Array.map toRemainder
    |> Array.sortByDescending (fun (n, a) -> n)

let common (n, x1) (x2, a2) =
    let mutable n = n
    while n % x2 <> a2 do
        n <- n + x1
    (n, x1 * x2)

let part2 () =
    Array.fold common (snd remainders.[0], fst remainders.[0]) remainders.[1..]

printfn "Part 2: %A" (fst (part2 ()))
