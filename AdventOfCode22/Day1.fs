module AdventOfCode22.Day1

open System
open Helpers


let valueMap (x: string) =
    let valid, v = Int32.TryParse x
    if valid then
        Some v
    else
        None

let day1Part1 =
    readMap 1 1 valueMap
    // |> toMappedLines valueMap
    |> Seq.fold (fun (acc: int list) (curr: int option) ->
        match curr with
        | None -> 0::acc
        | Some v ->
            match acc with
            | x::xs -> (x+v)::xs
            | [] -> [v]
    ) List.empty<int>
    |> Seq.max

let day1Part2 =
    readMap 1 1 valueMap
    // |> toMappedLines valueMap
    |> Seq.fold (fun (acc: int list) (curr: int option) ->
        match curr with
        | None -> 0::acc
        | Some v ->
            match acc with
            | x::xs -> (x+v)::xs
            | [] -> [v]
    ) List.empty<int>
    |> List.sortDescending
    |> List.take 3
    |> List.sum
