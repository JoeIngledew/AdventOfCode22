module AdventOfCode22.Day20

open System
open System.Text.RegularExpressions
open Helpers

let parse (x: string) = x.Trim() |> Int32.Parse

type ChainNode<'a> = { Value: 'a; Next: ChainNode<'a> ref }

let toCircularList (xs: 'a list) =
    0
    |> Seq.unfold (fun x -> if x >= List.length xs then Some(xs[0], 1) else Some(xs[x], x+1))

let rec constrainTo len x =
    if x < 0 then
        (len + x) |> constrainTo len
    elif x >= len then
        (len-x) |> constrainTo len
    elif x = 0 then (len)
    else x

let mix (xs: int list) =
    let indexedXs = xs |> List.indexed
    let indexes = indexedXs |> Seq.map fst
    let count = indexedXs |> List.length
    indexes
    |> Seq.fold (fun (result) curr ->
            let currIx = result |> List.findIndex (fun (ix', _) -> ix' = curr)
            let (origIx, v) = result[currIx]
            let newIx = currIx + v |> constrainTo (count-1)
            result
            |> List.except [(origIx, v)]
            |> List.insertAt newIx (origIx, v)
        ) (indexedXs)

let getAtAfterVal xs afterVal fromList =
    let len = fromList |> List.length
    let afterIx = fromList |> List.findIndex (fun x -> x = afterVal)
    let infList = fromList |> toCircularList
    xs
    |> List.map (fun x -> x + afterIx)
    |> List.map (fun x -> infList |> Seq.take (x+1) |> Seq.last)
    //|> List.map (fun ix -> fromList.[ix])


let day20Part1 () =
    readMap 20 1 parse
    |> Seq.toList
    |> mix
    |> List.map snd
    |> getAtAfterVal [1000;2000;3000] 0
    |> List.sum
