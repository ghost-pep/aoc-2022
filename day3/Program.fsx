let input =
    System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}/input.txt")
    |> Seq.map (fun s -> s.Trim())
    |> List.ofSeq

let splitBags (line: string) =
    let mid = line.Length / 2
    let bag1 = line[.. mid - 1] |> Set
    let bag2 = line[mid..] |> Set
    [ bag1; bag2 ]

let score set =
    set
    |> Seq.map (fun c ->
        if System.Char.ToUpper c = c then
            int c - int 'A' + 27
        else
            int c - int 'a' + 1)
    |> Seq.sum

let bagDiff (sets: Set<char> list) =
    if sets.IsEmpty then
        Set.empty
    else
        Set.intersectMany sets

let sets = input |> Seq.map splitBags
let diffs = sets |> Seq.map Set.intersectMany
let scores = diffs |> Seq.map score

let rec getGroups (lines: string list) =
    if (lines.Length < 3) then
        [ lines ]
    else
        [ (lines |> List.take 3) ]
        @ getGroups (lines |> List.skip 3)

let groups = input |> getGroups

let groupSets = groups |> List.map (List.map Set)

let groupDiffs =
    groupSets
    |> List.map bagDiff
    |> List.filter (fun s -> s.IsEmpty |> not)

let groupScores = groupDiffs |> Seq.map score

let problem1 = scores |> Seq.sum
let problem2 = groupScores |> Seq.sum

(problem1, problem2)
