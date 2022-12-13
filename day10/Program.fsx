let input =
    System
        .IO
        .File
        .ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")
        .Trim()

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

type Instruction =
    | Noop
    | Add of int
    | Add2 of int

type State = { Cycle: int; X: int }

let parseLine (line: string) =
    match line with
    | "noop" -> Noop
    | Prefix "addx " num -> Add(num |> int)

let parse (input: string) =
    input.Split('\n')
    |> List.ofArray
    |> List.map parseLine

let rec runInstruction state instruction =
    match instruction with
    | Noop -> { state with Cycle = state.Cycle + 1 }
    | Add x -> runInstruction { state with Cycle = state.Cycle + 1 } (Add2 x)
    | Add2 x ->
        { state with
            Cycle = state.Cycle + 1
            X = state.X + x }

let run instructions =
    instructions
    |> List.fold runInstruction { Cycle = 1; X = 1 }

parse input |> List.take 3 |> run
