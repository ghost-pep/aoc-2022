let input =
    System
        .IO
        .File
        .ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")
        .Trim()

let test = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

let msgIdx len (msg: string) =
    msg
    |> Seq.mapi (fun i _ ->
        msg[i .. msg.Length]
        |> Seq.chunkBySize len
        |> Seq.head)
    |> Seq.findIndex (fun s ->
        let n_unique = s |> Seq.distinct |> Seq.length
        n_unique = len)
    |> (fun n -> n + len)

input |> msgIdx 4
input |> msgIdx 14
