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

type State = { X: int }

let parseLine (line: string) =
    match line with
    | "noop" -> Noop
    | Prefix "addx " num -> Add(num |> int)

let parse (input: string) =
    input.Split('\n')
    |> List.ofArray
    |> List.map parseLine

let rec runInstruction states instruction =
    match instruction with
    | Noop -> List.head states :: states
    | Add x -> runInstruction (List.head states :: states) (Add2 x)
    | Add2 x ->
        let newState = List.head states
        { newState with X = newState.X + x } :: states

let run instructions =
    instructions
    |> List.fold runInstruction [ { X = 1 } ]

let sumStates (states: State list) =
    let interestingIdxs = [ 20; 60; 100; 140; 180; 220 ]
    let reversed = states |> List.rev

    interestingIdxs
    |> List.map (fun idx -> List.tryItem (idx - 1) reversed) // subtract by 1 for 0 based indexing
    |> List.map (Option.defaultValue { X = 0 })
    |> List.map (fun s -> s.X)
    |> List.zip interestingIdxs
    |> List.map (fun (cycle, x) -> cycle * x)
    |> List.sum

parse input |> run |> sumStates
