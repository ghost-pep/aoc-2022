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

let test2 =
    System
        .IO
        .File
        .ReadAllText($"{__SOURCE_DIRECTORY__}/test2.txt")
        .Trim()

let parseLine (line: string) =
    let split = line.Split(' ')
    let direction = split[0]
    let length = split[1] |> int
    (direction, length)

let parse (input: string) =
    input.Split('\n')
    |> List.ofArray
    |> List.map parseLine

type Knot = { X: int; Y: int }

type State =
    { Knots: Knot list
      Visited: (int * int) list }

let startingKnot = { X = 0; Y = 0 }

let startingStateN n =
    { Knots = List.replicate n startingKnot
      Visited = [] }

let moveTail tail head =
    let xDiff = head.X - tail.X
    let yDiff = head.Y - tail.Y

    if xDiff > 1 && yDiff = 0 then // case for right
        { X = tail.X + 1; Y = tail.Y }
    elif xDiff < -1 && yDiff = 0 then // case for left
        { X = tail.X - 1; Y = tail.Y }
    elif yDiff > 1 && xDiff = 0 then // case for up
        { X = tail.X; Y = tail.Y + 1 }
    elif yDiff < -1 && xDiff = 0 then // case for down
        { X = tail.X; Y = tail.Y - 1 }
    elif xDiff > 1 && yDiff = 1 then // case for up and right to meet a right head
        { X = tail.X + 1; Y = tail.Y + 1 }
    elif xDiff = 1 && yDiff > 1 then // case for up and right to meet an up head
        { X = tail.X + 1; Y = tail.Y + 1 }
    elif xDiff < -1 && yDiff = 1 then // case for up and left to meet a left head
        { X = tail.X - 1; Y = tail.Y + 1 }
    elif xDiff = -1 && yDiff > 1 then // case for up and left to meet an up head
        { X = tail.X - 1; Y = tail.Y + 1 }
    elif xDiff < -1 && yDiff = -1 then // case for down and left to meet a left head
        { X = tail.X - 1; Y = tail.Y - 1 }
    elif xDiff = -1 && yDiff < -1 then // case for down and left to meet a down head
        { X = tail.X - 1; Y = tail.Y - 1 }
    elif xDiff > 1 && yDiff = -1 then // case for down and right to meet a right head
        { X = tail.X + 1; Y = tail.Y - 1 }
    elif xDiff = 1 && yDiff < -1 then // case for down and right to meet a down head
        { X = tail.X + 1; Y = tail.Y - 1 }
    elif xDiff > 1 && yDiff > 1 then // case for up and right for both
        { X = tail.X + 1; Y = tail.Y + 1 }
    elif xDiff < -1 && yDiff > 1 then // case for up and left for both
        { X = tail.X - 1; Y = tail.Y + 1 }
    elif xDiff > 1 && yDiff < -1 then // case for down and right for both
        { X = tail.X + 1; Y = tail.Y - 1 }
    elif xDiff < -1 && yDiff < -1 then // case for down and left for both
        { X = tail.X - 1; Y = tail.Y - 1 }
    else // no change
        { X = tail.X; Y = tail.Y }

let rec newKnots newHead list =
    match list with
    | [] -> []
    | [ x ] -> [ moveTail x newHead ]
    | x :: xs ->
        let newX = moveTail x newHead
        newX :: newKnots newX xs


let rec simulate (state: State) (direction, length) =
    let head = state.Knots |> List.head
    let tail = state.Knots |> List.tail

    let newState newHead =
        let newTail = newKnots newHead tail
        let newLast = newTail |> List.last

        { Knots = newHead :: newTail
          Visited = (newLast.X, newLast.Y) :: state.Visited }

    if length = 0 then
        state
    else
        match direction with
        | "R" ->
            let newHead = { X = head.X + 1; Y = head.Y }

            simulate (newState newHead) (direction, length - 1)
        | "L" ->
            let newHead = { X = head.X - 1; Y = head.Y }

            simulate (newState newHead) (direction, length - 1)
        | "U" ->
            let newHead = { X = head.X; Y = head.Y + 1 }

            simulate (newState newHead) (direction, length - 1)
        | "D" ->
            let newHead = { X = head.X; Y = head.Y - 1 }

            simulate (newState newHead) (direction, length - 1)


input
|> parse
|> List.fold simulate (startingStateN 10)
|> fun state -> state.Visited |> Set
|> Set.count
