let input =
    System
        .IO
        .File
        .ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")
        .Trim()

let test =
    System
        .IO
        .File
        .ReadAllText($"{__SOURCE_DIRECTORY__}/test.txt")
        .Trim()

let parseLine l =
    l |> Array.ofSeq |> Array.map (string >> int)

let parseGrid (g: string) = g.Split('\n') |> Array.map parseLine

let isVisible (grid: int array array) (x, y) =
    let row = grid[y]
    let col = grid |> Array.map (fun r -> r[x])
    let height = row[x]

    let vis (s: int array) idxs =
        idxs
        |> List.map (fun i -> s[i])
        |> List.fold (fun vis i -> vis && i < height) true

    let visLeft = [ 0 .. x - 1 ] |> vis row
    let visRight = [ x + 1 .. row.Length - 1 ] |> vis row
    let visAbove = [ 0 .. y - 1 ] |> vis col
    let visBelow = [ y + 1 .. col.Length - 1 ] |> vis col

    visLeft || visRight || visAbove || visBelow

let numVisible (grid: int array array) =
    let nRows = Array.length grid[0]
    let nCols = Array.length grid
    let nOutter = (nRows * 2) + (nCols - 2) * 2
    let innerX = [ 1 .. nRows - 2 ]
    let innerY = [ 1 .. nCols - 2 ]
    let innerPairs = List.allPairs innerX innerY

    let nInner =
        innerPairs
        |> List.map (isVisible grid)
        |> List.filter (fun x -> x)
        |> List.length

    nOutter + nInner

let scorePair (grid: int array array) (x, y) =
    let row = grid[y]
    let col = grid |> Array.map (fun r -> r[x])
    let height = row[x]

    let scoreDirection (s: int array) idxs =
        idxs
        |> List.map (fun i -> s[i])
        |> List.fold
            (fun (score, isDone) i ->
                if not isDone then
                    if i < height then
                        (score + 1, false)
                    else
                        (score + 1, true)
                else
                    (score, true))
            (0, false)
        |> fst

    let scoreLeft = [ 0 .. x - 1 ] |> List.rev |> scoreDirection row
    let scoreRight = [ x + 1 .. row.Length - 1 ] |> scoreDirection row
    let scoreAbove = [ 0 .. y - 1 ] |> List.rev |> scoreDirection col
    let scoreBelow = [ y + 1 .. col.Length - 1 ] |> scoreDirection col
    scoreLeft * scoreRight * scoreAbove * scoreBelow

let bestScenicScore (grid: int array array) =
    let nRows = Array.length grid[0]
    let nCols = Array.length grid
    let innerX = [ 1 .. nRows - 2 ]
    let innerY = [ 1 .. nCols - 2 ]
    let innerPairs = List.allPairs innerX innerY

    innerPairs
    |> List.map (scorePair grid)
    |> List.max


input |> parseGrid |> bestScenicScore
