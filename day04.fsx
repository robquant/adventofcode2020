open System.IO
open System.Text.RegularExpressions

let raw = File.ReadAllText("day04.txt")

let passports =
    Regex.Split(raw, "\n\n")
    |> Array.map (fun s -> s.Split(' ', '\n'))

let required =
    [| "byr"
       "iyr"
       "eyr"
       "hgt"
       "hcl"
       "ecl"
       "pid" |]

let isValid (passport: string array) =
    let pairs =
        passport
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map (fun s -> s.Split(':'))

    let keys =
        pairs |> Seq.map (fun p -> p.[0], p.[1]) |> dict

    if Array.forall (fun k -> keys.ContainsKey(k)) required
    then 1
    else 0

let validPassports = passports |> Array.sumBy isValid
printfn "Part 1: %d" validPassports
