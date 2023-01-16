module AdventOfCode22.Day14

open System
open System.Text.RegularExpressions
open Helpers

let processInputs (xs: string list) =
    let coords =
        xs
        |> List.map (fun s ->
            let pairPattern = "([0-9]+),([0-9]+)"
            let matcher = Regex pairPattern
            let matches = matcher.Matches s
            [ for m in matches ->
                let [x;y] = List.tail [ for g in m.Groups -> (Int32.Parse g.Value) ]
                (x,y)
            ])
    let initial = Seq.initInfinite (fun _ -> Seq.initInfinite (fun _ -> false))
    initial

