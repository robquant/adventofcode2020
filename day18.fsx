let rec parse (input: string) (stack: char list) =
    match input with
    | "" -> stack
    | _ -> parse input.[1..] (input.[0] :: stack)


printfn "%A" (parse "Hello" [])

let input = "3 + (8"
input.Replace("(", "( ")
