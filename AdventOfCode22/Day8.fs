module AdventOfCode22.Day8

open System
open Helpers

let toHeightMapRow (s: string) =
    [ for c in s -> c |> sprintf "%c" |> Int32.Parse ]

let getHeightMap part =
    let jaggedArr =
        readMap 8 part toHeightMapRow
        |> Seq.toArray
        |> Array.map List.toArray
    Array2D.init jaggedArr.[0].Length jaggedArr.Length (fun x y -> jaggedArr.[y].[x])

open System.Collections.Generic

// TODO
let findVisibleCells (heightmap: int[,]) =
    let mutable cellsVisible = List<(int * int)> ()
    let width = heightmap.GetLength(0)
    let height = heightmap.GetLength(1)
    for y in 0..height - 1 do
        let mutable max = -1
        for x in 0..width - 1 do
            if heightmap[x,y] > max then
                max <- heightmap[x,y]
                cellsVisible.Add(x,y)

    for x in 0..width - 1 do
        let mutable max = -1
        for y in 0..height - 1 do
            if heightmap[x,y] > max then
                max <- heightmap[x,y]
                cellsVisible.Add(x,y)

    let revXs = [0..width - 1]  |> List.rev
    let revYs = [0..height - 1] |> List.rev
    for y in revYs do
        let mutable max = -1
        for x in revXs do
            if heightmap[x,y] > max then
                max <- heightmap[x,y]
                cellsVisible.Add(x,y)

    for x in revXs do
        let mutable max = -1
        for y in revYs do
            if heightmap[x,y] > max then
                max <- heightmap[x,y]
                cellsVisible.Add(x,y)
    [for i in cellsVisible -> i]
    |> List.distinct

let day8Part1 () =
    getHeightMap 1
    |> findVisibleCells
    |> List.length

let toScenicScores (heightmap: int[,]) =
    let height = heightmap.GetLength(1)
    let width = heightmap.GetLength(0)
    let getScenicScore (heightmap: int[,]) (x: int) (y: int) (v: int) =
        let getViewRange ls =
            ls
            |> List.fold (fun (obstructed, count) curr ->
                if obstructed then (obstructed, count)
                elif curr < v then (false, count+1)
                else (true, count+1)) (false, 0)
            |> snd
        let above =
            if y = 0 then [] else [0..(y-1)]
            |> List.map (fun y' -> heightmap[x,y'])
            |> List.rev
            |> getViewRange
        let below =
            if y = (height-1) then [] else [y+1..(height-1)]
            |> List.map (fun y' -> heightmap[x,y'])
            |> getViewRange
        let left =
            if x = 0 then [] else [0..(x-1)]
            |> List.map (fun x' -> heightmap[x',y])
            |> List.rev
            |> getViewRange
        let right =
            if x = (width-1) then [] else [x+1..(width-1)]
            |> List.map (fun x' -> heightmap[x',y])
            |> getViewRange
        above * below * left * right

    heightmap
    |> Array2D.mapi (getScenicScore heightmap)

let flatten (xys: 'a[,]) =
    xys
    |> Seq.cast<'a>

let day8Part2 () =
    getHeightMap 1
    |> toScenicScores
    |> flatten
    |> Seq.sortDescending
    |> Seq.find (fun _ -> true) // equivalent to "first"
