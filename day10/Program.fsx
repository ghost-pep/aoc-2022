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

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

type Instruction =
    | Noop
    | Add of int
    | Add2 of int

type CRT = { Buffer: char array; CurIdx: int }

let emptyCRT =
    { Buffer = Array.create 240 ' '
      CurIdx = 0 }

type State = { X: int; Monitor: CRT }

let parseLine (line: string) =
    match line with
    | "noop" -> Noop
    | Prefix "addx " num -> Add(num |> int)

let parse (input: string) =
    input.Split('\n')
    |> List.ofArray
    |> List.map parseLine

let updateCRT state =
    let curBuffer = Array.copy state.Monitor.Buffer
    let curIdx = state.Monitor.CurIdx
    let curXIdx = curIdx % 40
    let curX = state.X

    if curXIdx >= curX - 1 && curXIdx <= curX + 1 then
        curBuffer.SetValue('#', curIdx)
    else
        curBuffer.SetValue('.', curIdx)

    { Buffer = curBuffer
      CurIdx = curIdx + 1 }

let rec runInstruction states instruction =
    let curState = List.head states
    let newState = { curState with Monitor = updateCRT curState }

    match instruction with
    | Noop -> newState :: states
    | Add x -> runInstruction (newState :: states) (Add2 x)
    | Add2 x -> { newState with X = newState.X + x } :: states

let run instructions =
    instructions
    |> List.fold runInstruction [ { X = 1; Monitor = emptyCRT } ]

let sumStates (states: State list) =
    let interestingIdxs = [ 20; 60; 100; 140; 180; 220 ]
    let reversed = states |> List.rev

    interestingIdxs
    |> List.map (fun idx -> List.tryItem (idx - 1) reversed) // subtract by 1 for 0 based indexing
    |> List.map (Option.defaultValue { X = 0; Monitor = emptyCRT })
    |> List.map (fun s -> s.X)
    |> List.zip interestingIdxs
    |> List.map (fun (cycle, x) -> cycle * x)
    |> List.sum

let visualizeCRT state =
    for y in 0..5 do
        for x in 0..39 do
            let idx = x + (y * 40)
            printf "%c" state.Monitor.Buffer[idx]

        printf "\n"

parse input |> run |> List.head |> visualizeCRT
