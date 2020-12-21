module Bench

open System.IO
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

let findTripleArray items =
    let bag = Array.zeroCreate 2020

    for item in items do
        bag.[item] <- 1

    let inBag items =
        seq {
            for a in items do
                for b in items do
                    if (a + b < 2020) && (bag.[2020 - a - b] = 1)
                    then (a, b, 2020 - a - b)
        }

    inBag items |> Seq.head

let findTripleHashSet items =
    let bag = new HashSet<int>()

    for item in items do
        bag.Add item |> ignore

    let inBag items =
        seq {
            for a in items do
                for b in items do
                    if bag.Contains(2020 - a - b) then (a, b, 2020 - a - b)
        }

    inBag items |> Seq.head

let findTripleSet items =
    let bag = Set.ofList items

    let inBag items =
        seq {
            for a in items do
                for b in items do
                    if bag.Contains(2020 - a - b) then (a, b, 2020 - a - b)
        }

    inBag items |> Seq.head

let findTriplePairs items =
    let sumIs2020 (a, b, c) = (a + b + c = 2020)

    let result =
        seq {
            for a in items do
                for b in items do
                    for c in items do
                        a, b, c
        }
        |> Seq.find sumIs2020

    result

type FindTripleComparison() =

    let path =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "day01.txt")

    member self.mainList =
        File.ReadLines(path) |> Seq.map int |> Seq.toList

    [<Benchmark>]
    member self.FindTripleHashSet() = findTripleHashSet self.mainList

    [<Benchmark>]
    member self.FindTripleSet() = findTripleSet self.mainList

    [<Benchmark>]
    member self.FindTripleArray() = findTripleArray self.mainList

    [<Benchmark>]
    member self.FindTriplePairs() = findTriplePairs self.mainList



[<EntryPoint>]
let main argv =

    let bench () =
        BenchmarkRunner.Run<FindTripleComparison>()

    bench () |> ignore
    0
