let input =
    System
        .IO
        .File
        .ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")
        .Trim()

let msgs =
    input.Split("$")
    |> Array.tail
    |> Array.map (fun s -> s.Trim())

type INode =
    | Directory of Map<string, INode>
    | File of int

let emptyNode = Directory(Map.empty)

let emptyFs = Directory(Map [ ("/", emptyNode) ])

let parseCD (pwd, fs) dir =
    match dir with
    | "/" -> ([ "/" ], fs)
    | ".." -> (List.tail pwd, fs)
    | x -> (x :: pwd, fs)

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let parseListing line =
    match line with
    | Prefix "dir " name -> (name, emptyNode)
    | x ->
        let sp = x.Split(' ')
        let size = sp[0] |> int
        let name = sp[1]
        (name, File(size))

let rec parseLS (pwd: string list, fs) out =
    match pwd |> List.rev with
    | [ dir ] ->
        let children = out |> Seq.map parseListing |> Map

        match fs with
        | Directory m ->
            let node = m.Add(dir, Directory children)
            (pwd, Directory node)
    | dir :: path ->
        match fs with
        | Directory m ->
            let node =
                m
                |> Map.find dir
                |> fun oldfs -> parseLS (path |> List.rev, oldfs) out
                |> fun (_, newfs) -> m.Add(dir, newfs)
                |> Directory

            (pwd, node)


let parseMsg state (msg: string) =
    let lines = msg.Split('\n')
    let cmdArr = (lines[0]).Split(' ')
    let cmd = cmdArr[0]

    match cmd with
    | "ls" -> parseLS state lines[1..]
    | "cd" -> parseCD state cmdArr[1]

let parseMsgs msgs = msgs |> Seq.fold parseMsg ([], emptyFs)

msgs |> parseMsgs
