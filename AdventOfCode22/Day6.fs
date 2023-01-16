module AdventOfCode22.Day6

let getCode  packetSize (s: string) =
    let chars = [for c in s -> c] |> List.toSeq
    let windows = chars |> Seq.windowed packetSize
    let firstWindowMatchIx =
        windows
        |> Seq.findIndex (fun cs ->
            let distinct = cs |> Array.distinct
            distinct.Length = cs.Length)
    firstWindowMatchIx + packetSize // add on 4 as the last character ix of the window is the intended result

open Helpers

let day6Part1 =
    readInput 6 1
    |> getCode 4

let day6Part2 =
    readInput 6 1 |> getCode 14
