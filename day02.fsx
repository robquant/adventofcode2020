open System.IO

type Criterium =
    { MinRep: int
      MaxRep: int
      Letter: char }

let count x = Seq.filter ((=) x) >> Seq.length

let isValidPart1 (criterium: Criterium) s =
    let occ = count criterium.Letter s

    (occ >= criterium.MinRep)
    && (occ <= criterium.MaxRep)

let isValidPart2 (criterium: Criterium) (s: string) =
    let first =
        (s.[criterium.MinRep - 1] = criterium.Letter)

    let second =
        (s.[criterium.MaxRep - 1] = criterium.Letter)

    (first || second) && not (first && second)

let parseLine (line: string) =
    let items = line.Split(" ")
    let reps = items.[0].Split("-")

    { MinRep = int reps.[0]
      MaxRep = int reps.[1]
      Letter = items.[1].[0] },
    items.[2]

let input =
    File.ReadAllLines("day02.txt")
    |> Array.map parseLine

let part1 =
    input
    |> Seq.filter (fun (crit, str) -> isValidPart1 crit str)
    |> Seq.length

let part2 =
    input
    |> Seq.filter (fun (crit, str) -> isValidPart2 crit str)
    |> Seq.length

printfn "Part1: %d" part1
printfn "Part2: %d" part2
