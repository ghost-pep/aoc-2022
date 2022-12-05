let input =
    System
        .IO
        .File
        .ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")
        .Split("\n\n")

let starting = input[0]
let moves_raw = input[1]

let parseStarting (s: string) =
    s.Split('\n') |> (fun a -> a[0 .. a.Length - 2])

let nbins =
    starting.Split('\n')
    |> (fun a ->
        let s = a[a.Length - 1]
        s[s.Length - 2])
    |> string
    |> int

let lines = parseStarting starting |> Array.rev

let parseLine (l: string) bins =
    let mutable cur = l
    let mutable saved_bin = 0

    while (cur.Trim() <> "") do
        printfn "%s" cur

        let idx = cur.IndexOf('[')
        let letter = cur[idx + 1]
        let num = (idx / 4) + saved_bin
        printfn "%d %c" num letter
        Array.set bins num ([ letter ] @ (Array.get bins num))
        printfn "%s" ((Array.get bins num).ToString())
        cur <- cur[idx + 4 ..]
        saved_bin <- num + 1

    bins

let parseLines ls =
    ls
    |> Array.fold (fun acc l -> parseLine l acc) (Array.replicate nbins [])

let state = parseLines lines

let parseMove (m: string) =
    let arr = m.Split(' ')
    let count = arr[1] |> int
    let fromBin = arr[3] |> int |> (fun c -> c - 1)
    let toBin = arr[5] |> int |> (fun c -> c - 1)
    (count, fromBin, toBin)

let parseMoves (ms: string) =
    ms.Split('\n')
    |> (fun x -> x[0 .. x.Length - 2])
    |> Array.map parseMove

let moves = parseMoves moves_raw

for (count, fromBin, toBin) in moves do
    //let (count, fromBin, toBin) = moves[0]
    let bin = state[fromBin]
    let cnt = min count bin.Length
    let kept = List.removeManyAt 0 count bin
    Array.set state fromBin kept
    let moved = bin[0 .. count - 1]
    // needed for task 1 execution to flip order preserving stack transfer
    // |> List.rev
    Array.set state toBin (moved @ state[toBin])
//printfn "FROM: %s" ((Array.get state fromBin).ToString())
//printfn "TO: %s" ((Array.get state toBin).ToString())


let final = state

final
|> Array.map List.head
|> Array.map string
|> Array.fold (fun acc s -> acc + s) ""
