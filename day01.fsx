open System.IO
open System.Collections.Generic


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

let path =
    Path.Combine(__SOURCE_DIRECTORY__, "day01.txt")

let items =
    System.IO.File.ReadLines path
    |> Seq.map int
    |> Seq.toList

let sumOf2is2020 (a: int, b: int) = (a + b = 2020)

let a, b =
    Seq.allPairs items items |> Seq.find sumOf2is2020

System.Console.WriteLine("Part 1: {0}", a * b)

let a2, b2, c2 = findTripleHashSet items
System.Console.WriteLine("Part 2: {0}", a2 * b2 * c2)
