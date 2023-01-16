module AdventOfCode22.Day10

open System
open System.Text.RegularExpressions
open Helpers

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

// Add: 2 cycles. NoOp: 1 cycle
type CpuInstruct =
| Add of v: int32
| NoOp

// signal strength = cycle num * val in x register

let isTrackCycle (cycle: int32) =
    cycle = 20 || (cycle - 20) % 40 = 0

let getSigStr (cycle: int32) (x: int32) = cycle * x

let toInstr (s: string) =
    match s with
    | Regex "addx (-?[0-9]+)" [v] -> Add (Int32.Parse v)
    | Regex "noop" []             -> NoOp
    | _                           -> failwith "unexpected input"

let processInstruction (xReg, cycle, acc) curr =
    let newX, newCycle, logX =
        match curr with
        | Add v ->
            if isTrackCycle (cycle + 1) then
                xReg + v, cycle + 2, Some (xReg * (cycle+1))
            elif isTrackCycle (cycle + 2) then
                xReg + v, cycle + 2, Some ((xReg + v)*(cycle+2))
            else xReg + v, cycle + 2, None
        | NoOp  ->
            if isTrackCycle (cycle + 1) then
                xReg, cycle + 1, Some (xReg*(cycle+1))
            else xReg, cycle + 1, None
    match logX with
    | Some v -> newX, newCycle, (acc + v)
    | None   -> newX, newCycle, acc

let day10Part1 () =
    readMap 10 1 toInstr
    |> Seq.fold processInstruction (1, 1, 0)
    |> (function (_, _, x) -> x)

let toCycleXVals (instructions: CpuInstruct seq) =
    instructions
    |> Seq.fold (fun (cycleXs, currX) curr ->
        match curr with
        | Add v ->
            let nextX = currX + v
            (nextX::currX::cycleXs), nextX
        | NoOp -> (currX::cycleXs), currX) ([], 1)
    |> fst
    |> List.rev

let isLit pixelCounter v =
    if v % 40 = 0 then // left edge
        pixelCounter = v || pixelCounter = (v+1)
    elif (v+1) % 40 = 0 then // right edge
        pixelCounter = v || pixelCounter = (v-1)
    else // neither edge
        pixelCounter = v || pixelCounter = (v-1) || pixelCounter = (v+1)


let day10Part2 () =
    let instructions = readMap 10 1 toInstr
    let cycleXVals = instructions |> toCycleXVals
    let pixels = List.init 240 id
    let pixelCycles = List.zip pixels (1::(cycleXVals |> List.rev |> List.skip 1 |> List.rev))
    let pixelLights =
        pixelCycles
        |> List.map (fun (p, x) ->
            let modifiedP =
                match p with
                | x when x > 199 -> x-200
                | x when x > 159 -> x-160
                | x when x > 119 -> x-120
                | x when x > 79  -> x-80
                | x when x > 39  -> x-40
                | x              -> x
            if isLit modifiedP x then "#" else ".")
    let groups = pixelLights |> List.splitInto 6
    for line in groups do
        for p in line do printf $"%s{p}"
        printf $"%s{Environment.NewLine}"

