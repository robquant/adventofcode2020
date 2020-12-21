open System.IO

let path =
    Path.Combine(__SOURCE_DIRECTORY__, "day03.txt")

let lines =
    File.ReadLines path
    |> Seq.map (fun line -> line.ToCharArray())
    |> Seq.toList

let height = List.length lines
let width = Array.length lines.[0]

let forrest =
    Array2D.init height width (fun i j -> lines.[i].[j])

let countTrees (step: int * int) =
    let mutable posX = 0
    let mutable posY = 0
    let tree = '#'
    let stepX, stepY = step
    let mutable treeCount: int64 = int64 0
    while posY < height do
        if forrest.[posY, posX] = tree then treeCount <- treeCount + int64 1
        posX <- (posX + stepX) % width
        posY <- posY + stepY
    treeCount

printfn "Part 1 : %d" (countTrees (3, 1))

let result =
    [| (1, 1)
       (3, 1)
       (5, 1)
       (7, 1)
       (1, 2) |]
    |> Seq.map countTrees
    |> Seq.reduce (fun x y -> (x * y))

printfn "Part 2: %d" result
