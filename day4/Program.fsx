let input =
    System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}/input.txt")
    |> Seq.map (fun s -> s.Trim())
    |> List.ofSeq

let sets =
    input
    |> List.map (fun l -> l.Split(',') |> List.ofArray)
    |> List.map (
        List.map (fun p ->
            match p.Split('-') with
            | [| a; b |] -> [ (int a) .. (int b) ] |> Set)
    )

let subsets =
    sets
    |> List.map (fun sets ->
        match sets with
        | [ a; b ] ->
            if Set.isSubset a b || Set.isSubset b a then
                1
            else
                0)

let overlaps =
    sets
    |> List.map (fun [ a; b ] -> Set.intersect a b |> Set.isEmpty |> not)
    |> Seq.sumBy (fun b -> if b then 1 else 0)

subsets |> Seq.sum
