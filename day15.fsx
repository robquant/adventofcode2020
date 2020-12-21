open System
open System.Collections.Generic

let input =
    System.IO.File.ReadAllLines("day15.txt").[0].Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int

// let input = [| 3; 1; 2 |]
let n = 30000000
let numbers = Array.create n 0
let memory = new Dictionary<int, int * int>()

for i = 0 to (input.Length - 1) do
    numbers.[i] <- input.[i]
    memory.[input.[i]] <- (i, i)

#time 
for i = input.Length to n - 1 do
    let prev = numbers.[i - 1]
    let mutable say = 0
    if memory.ContainsKey(prev) then
        let m = memory.[prev]
        say <- (fst m) - (snd m)
        numbers.[i] <- say
    else
        numbers.[i] <- 0

    if memory.ContainsKey(say) then memory.[say] <- (i, fst memory.[say]) else memory.[say] <- (i, i)

printfn "%d" numbers.[numbers.Length - 1]
