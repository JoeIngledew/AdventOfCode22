module AdventOfCode22.Day3

open Helpers

let toCompartments (s: string) =
    let compartmentSize = s.Length / 2
    (s.[0 .. (compartmentSize-1)], s.[compartmentSize ..])

let findMatchingItem (s1, s2) =
    let chars = [for c in s1 -> c]
    let character =
        chars
        |> List.filter (fun x -> String.exists (fun c -> c = x) s2)
        |> List.take 1
    character.[0]

let toPriority (c: char) =
    let lower = [ 'a' .. 'z' ]
    let upper = ['A' .. 'Z']
    let allChars = List.concat [lower; upper]
    (List.findIndex (fun x -> x = c) allChars) + 1
let day3Part1 =
    readMap 3 1 id
    |> Seq.map toCompartments
    |> Seq.map findMatchingItem
    |> Seq.map toPriority
    |> Seq.sum

let strToArr s = [for c in s -> c]

let toCommonItems (xs: string[]) =
    let finalStr =
        xs
        |> Array.fold (fun acc curr ->
                if acc = "" then
                    curr
                else
                    String.filter (fun c -> String.exists (fun c' -> c = c') curr) acc
            ) ""
        |> strToArr
        |> List.distinct
    if finalStr.Length <> 1 then
        let err = $"wrong matches: %d{finalStr.Length}"
        failwith err
    else finalStr[0]

let day3part2 =
    readMap 3 1 id
    |> Seq.chunkBySize 3
    |> Seq.map toCommonItems
    |> Seq.map toPriority
    |> Seq.sum

