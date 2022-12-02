module day2

let inputFile = "./input.txt"

let getInput =
    System.IO.File.ReadAllLines(inputFile)

type Move =
    | Rock
    | Paper
    | Scissors

let baseScore move =
    match move with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let parseMove move =
    match move with
    | ("A" | "X") -> Some(Rock)
    | ("B" | "Y") -> Some(Paper)
    | ("C" | "Z") -> Some(Scissors)
    | _ -> None

let parseMoveLine (line : string) =
    match (line.Split ' ') with
    | [|opponent; recommendation|] -> (parseMove opponent, parseMove recommendation)
    | _ -> (None, None)

type GameResult =
    | Win
    | Draw
    | Lose

let parseResult res =
    match res with
    | "X" -> Some(Lose)
    | "Y" -> Some(Draw)
    | "Z" -> Some(Win)
    | _ -> None

let parseResultLine (line : string) =
    match (line.Split ' ') with
    | [|opponent; recommendation|] -> (parseMove opponent, parseResult recommendation)
    | _ -> (None, None)

let winScore = 6
let drawScore = 3
let lossScore = 0

let gameResult tuple =
    match tuple with
    | (x, y) when x = y -> Draw
    | ((Rock, Paper) | (Paper, Scissors) | (Scissors, Rock)) -> Win
    | _ -> Lose

let gameScore tuple =
    match (gameResult tuple) with
    | Win -> winScore
    | Draw -> drawScore
    | Lose -> lossScore

let score moves =
    match moves with
    | (Some(x), Some(y)) -> (baseScore y) + gameScore (x, y)
    | _ -> 0

let resolveMove opponent res =
    match res with
    | Win -> match opponent with
             | Rock -> Paper
             | Paper -> Scissors
             | Scissors -> Rock
    | Draw -> opponent
    | Lose -> match opponent with
              | Rock -> Scissors
              | Paper -> Rock
              | Scissors -> Paper

let pickResultMove resMove =
    match resMove with
    | (Some(opponent), Some(res)) -> (Some(opponent), Some(resolveMove opponent res))
    | _ -> (None, None)
    

let scoreMoveLine = parseMoveLine >> score
let scoreResultLine = parseResultLine >> pickResultMove >> score

let lines = getInput
lines |> Array.map scoreMoveLine |> Array.sum |> printfn "%d"
lines |> Array.map scoreResultLine |> Array.sum |> printfn "%d"