open System
open System.Diagnostics
open System.Text.RegularExpressions

let runWithWatch f =
    let watch = Stopwatch.StartNew()
    let res = f ()
    watch.Stop()
    printfn "Took: %d ms" watch.ElapsedMilliseconds
    res

let splitByEmptyLine raw =
    Regex.Split(raw, "\n\n")
    |> Array.map (fun s -> s.Split('\n', StringSplitOptions.RemoveEmptyEntries))
