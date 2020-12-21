open System

type Operation =
    | NOP
    | ACC
    | JMP

type Instruction = { Op: Operation; Arg: int }

let parse (line: string) =
    let operation op =
        match op with
        | "nop" -> NOP
        | "acc" -> ACC
        | "jmp" -> JMP
        | e -> failwith $"Unknown operation {e}"

    let instruction (args: string array) =
        { Op = operation args.[0]
          Arg = int args.[1] }

    instruction (line.Split(" ", StringSplitOptions.RemoveEmptyEntries))

let program =
    System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}/day8.txt")
    |> Array.map parse

let rec step (ip: int) (acc: int) (program: Instruction array) (visited: Set<int>) =
    if visited.Contains(ip) || (ip = program.Length) then
        (acc, ip)
    else
        let instr = program.[ip]
        let visited = visited.Add(ip)
        match instr.Op with
        | NOP -> step (ip + 1) acc program visited
        | ACC -> step (ip + 1) (acc + instr.Arg) program visited
        | JMP -> step (ip + instr.Arg) acc program visited

let run program = step 0 0 program Set.empty

#time
let (part1, _) = run program
printfn "Part 1: %d" part1

let mutatedSeq (program: Instruction array) =
    let mutate (program: Instruction array) i =
        let instr = program.[i]
        match instr.Op with
        | NOP -> program.[i] <- { program.[i] with Op = JMP }
        | JMP -> program.[i] <- { program.[i] with Op = NOP }
        | ACC -> ()
        ()

    seq {
        for i = 0 to (program.Length - 1) do
            let instr = program.[i]
            match instr.Op with
            | NOP ->
                mutate program i
                yield Some program
                mutate program i
            | JMP ->
                mutate program i
                yield Some program
                mutate program i
            | ACC -> yield None
    }

let falloff program =
    match run program with
    | (acc, ip) when ip = program.Length -> Some acc
    | _ -> None

let part2 =
    mutatedSeq program
    |> Seq.choose id
    |> Seq.choose falloff
    |> Seq.head

printfn "Part 2: %A" part2
// |> Seq.choose id
// |> Seq.map (fun p -> printfn "%A" p)
