open System
open System.IO

#load "Common.fsx"
open Common

let input =
    splitByEmptyLine (File.ReadAllText("day6.txt"))

let countDistinct group =
    Array.map Set.ofSeq group
    |> Set.unionMany
    |> Set.count

let countCommon group =
    Array.map Set.ofSeq group
    |> Set.intersectMany
    |> Set.count

let sum1 groups =
    Array.map countDistinct groups |> Array.sum

let sum2 groups =
    Array.map countCommon groups |> Array.sum

#time
let part1 = (fun _ -> sum1 input)
let part2 = (fun _ -> sum2 input)
printfn "Part 1: %d" (runWithWatch part1)
printfn "Part 2: %d" (runWithWatch part2)
