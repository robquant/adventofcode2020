open System
open System.IO

#load "Common.fsx"
open Common

let solve input =
    let binary c =
        match c with
        | 'B'
        | 'R' -> "1"
        | 'F'
        | 'L' -> "0"
        | _ -> "E"

    let toBinaryString s = String.collect binary s
    let binaryStringToInt s = Convert.ToInt32(s, 2)

    let seatIDs =
        input
        |> Array.map (toBinaryString >> binaryStringToInt)

    let minSeatID = Array.min seatIDs
    let maxSeatID = Array.max seatIDs

    let missing =
        Set.difference (Set.ofSeq [ minSeatID .. maxSeatID ]) (set seatIDs)
        |> Seq.head

    maxSeatID, missing

#time
let lines = File.ReadAllLines("day5.txt")
let maxSeatID, missing = runWithWatch (fun _ -> solve lines)
printfn "Part 1: %d" maxSeatID
printfn "Part 2: %d" missing
