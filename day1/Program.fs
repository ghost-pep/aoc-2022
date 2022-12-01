module day1

let inputFile = "./input.txt"

let getInput =
    System.IO.File.ReadAllLines(inputFile)

let elfPacksHelper =
    Array.fold (fun acc calorie -> 
        if calorie = "" then 
            (0, (acc |> fst) :: (acc |> snd))
        else
            ((acc |> fst) + (calorie |> int),
            acc |> snd)
    ) (0, List.Empty)

let elfPacks = elfPacksHelper >> snd

[<EntryPoint>]
let main argv =
    let lines = getInput
    let packs = lines |> elfPacks
    let maxPack = packs |> List.max
    let topThreePacks = packs |> List.sort |> List.rev |> List.take 3
    printfn "%d" maxPack
    printfn "%d" (List.sum topThreePacks)
    0
