let input =
    System
        .IO
        .File
        .ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")
        .Trim()

type Monkey =
    { Identifier: int
      StartingItems: int list
      Operation: int -> int
      Test: int -> bool
      TrueTarget: int
      FalseTarget: int }

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let parseIdentifier (line: string) =
    let withColon = line.Split(' ')[1]
    withColon[0 .. withColon.Length - 2] |> int

let parseStartingItems (line: string) =
    let items =
        match line with
        | Prefix "  Starting items: " x -> x

    items.Split(", ") |> List.ofArray |> List.map int

let parseOperation (line: string) =
    let body =
        match line with
        | Prefix "  Operation: new = " op -> op.Split(' ')

    let op =
        match body[1] with
        | "+" -> fun x y -> x + y
        | "*" -> fun x y -> x * y

    fun x ->
        op
            x
            (match body[2] with
             | "old" -> x
             | y -> y |> int)

let parseTest (line: string) =
    match line with
    | Prefix "  Test: divisible by " a -> fun x -> x % (int a) = 0

let parseTarget (line: string) =
    match line with
    | Prefix "    If true: throw to monkey " x -> x |> int
    | Prefix "    If false: throw to monkey " x -> x |> int

let parseMonkey (data: string) =
    let arr = data.Split('\n')
    let identifier = parseIdentifier arr[0]
    let startingItems = parseStartingItems arr[1]
    let operation = parseOperation arr[2]
    let test = parseTest arr[3]
    let trueTarget = parseTarget arr[4]
    let falseTarget = parseTarget arr[5]

    { Identifier = identifier
      StartingItems = startingItems
      Operation = operation
      Test = test
      TrueTarget = trueTarget
      FalseTarget = falseTarget }

let parse (text: string) =
    text.Split("\n\n") |> Array.map parseMonkey

input |> parse
