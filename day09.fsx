open System.IO


let input =
    File.ReadAllLines("day9.txt") |> Array.map uint64

let allPairsMap (arr: uint64 array) (f: (uint64) -> (uint64) -> (uint64)) =
    seq {
        for i in 0 .. (arr.Length - 1) do
            for j in (i + 1) .. (arr.Length - 1) do
                (f arr.[i] arr.[j])
    }

let canBeSum (number: uint64) (arr: uint64 array) =
    allPairsMap arr (fun a b -> a + b)
    |> Seq.contains number

let lastCanBeSum (arr: uint64 array) =
    let preamble = arr.[..arr.Length - 2]
    let number = arr.[arr.Length - 1]
    canBeSum number preamble

let cumsum arr = (Array.scan (+) 0UL arr).[1..]

#time

let part1 input =
    let arr =
        Array.windowed 26 input
        |> Seq.filter (fun arr -> not (lastCanBeSum arr))
        |> Seq.head

    arr.[arr.Length - 1]

let part2 input target =
    let cum = cumsum input
    let mutable startI = 1
    let mutable endI = 1
    while (cum.[endI] - cum.[startI - 1] <> target) do
        let sum = cum.[endI] - cum.[startI - 1]
        if sum < target then endI <- endI + 1
        if sum > target then startI <- startI + 1
    let range = input.[startI..endI]
    Array.min range + Array.max range

let resultPart1 = (part1 input)
printfn "%A" resultPart1
let resultPart2 = part2 input resultPart1
printfn "%A" resultPart2
()
