module AdventOfCode22.Day9

open System
open System.Text.RegularExpressions
open Helpers

type Point = int32 * int32

type RopeState = {
    HCoord: Point
    TCoord: Point
    THistory: Point list
}

type HDirect =
| Up of uint32
| Down of uint32
| Left of uint32
| Right of uint32

let followInstruction (s: RopeState) (curr: HDirect) =
    let hTransforms =
        match curr with
        | Up x    -> function (pX,pY) -> [for y' in 1u..x -> (pX, pY+(int32)y')]
        | Down x  -> function (pX,pY) -> [for y' in 1u..x -> (pX, pY-(int32)y')]
        | Left x  -> function (pX,pY) -> [for x' in 1u..x -> (pX-(int32)x', pY)]
        | Right x -> function (pX,pY) -> [for x' in 1u..x -> (pX+(int32)x', pY)]
    let hCoords = s.HCoord |> hTransforms
    let tCoord, tailTransforms =
        hCoords
        |> List.fold (fun ((tX, tY), acc: Point list) (hX, hY) ->
                let isTouching =
                    tX < (hX+2) && tX > (hX-2) && tY < (hY+2) && tY > (hY-2)
                let newT =
                    if isTouching then tX,tY
                    else
                        if tX = hX && tY < hY then tX, (tY+1)
                        elif tX = hX && tY > hY then tX, (tY-1)
                        elif tX < hX && tY = hY then (tX+1),tY
                        elif tX > hX && tY = hY then (tX-1),tY
                        elif tX < hX && tY < hY then (tX+1),(tY+1)
                        elif tX > hX && tY > hY then (tX-1),(tY-1)
                        elif tX < hX && tY > hY then (tX+1),(tY-1)
                        else (tX-1),(tY+1)
                    // if tX = hX && tY < (hY - 1) then (tX, tY+1)
                    // elif tX = hX && tY > (hY + 1) then (tX, tY-1)
                    // elif tY = hY && tX < (hX - 1) then (tX+1, tY)
                    // elif tY = hY && tX > (hX + 1) then (tX-1, tY)
                    // elif (tX = (hX-1) && tY = (hY-1)) || (tX = (hX+1) && tY = (hY+1)) || (tX = (hX-1) && tY = (hY+1)) || (tX = (hX+1) && tY = (hY-1)) then (tX, tY)
                    // elif tX < hX && tY < hY then (tX+1, tY+1)
                    // elif tX > hX && tY > hY then (tX-1, tY-1)
                    // elif tX > hX && tY < hY then (tX+1, tY-1)
                    // elif tX < hX && tY > hY then (tX-1, tY+1)
                    // else tX,tY// failwith $"what? tX: %d{tX} tY: %d{tY} hX: %d{hX} hY: %d{hY}"
                (newT, newT::acc)
            ) (s.TCoord, [])
    {
        HCoord = hCoords |> List.rev |> List.head
        TCoord = tCoord
        THistory = List.concat [ tailTransforms; s.THistory ]
    }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseInstruct (s: string) =
    match s with
    | Regex "([UDLR]{1}) ([0-9]+)" [direction; count] ->
        let intCount = UInt32.Parse count
        match direction with
        | "U" -> Up intCount
        | "D" -> Down intCount
        | "L" -> Left intCount
        | "R" -> Right intCount
        | _ -> failwith $"Unknown direction %s{direction}"
    | _ -> failwith $"Failed for parse: %s{s}"

let getDistinctTailCoords (s: RopeState) =
    s.THistory
    |> List.distinct

let day9Part1 () =
    readMap 9 1 parseInstruct
    |> Seq.fold followInstruction { HCoord = (0,0); TCoord = (0,0); THistory = [] }
    |> getDistinctTailCoords
    |> List.length
