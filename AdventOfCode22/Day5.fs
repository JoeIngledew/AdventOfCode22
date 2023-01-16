module AdventOfCode22.Day5

open System
open System.Text.RegularExpressions

type Stack = char list
type Stacks = Stack list

//                     [L]     [H] [W]
//                 [J] [Z] [J] [Q] [Q]
// [S]             [M] [C] [T] [F] [B]
// [P]     [H]     [B] [D] [G] [B] [P]
// [W]     [L] [D] [D] [J] [W] [T] [C]
// [N] [T] [R] [T] [T] [T] [M] [M] [G]
// [J] [S] [Q] [S] [Z] [W] [P] [G] [D]
// [Z] [G] [V] [V] [Q] [M] [L] [N] [R]

let startingPos = [
    ['S';'P';'W';'N';'J';'Z']
    ['T';'S';'G']
    ['H';'L';'R';'Q';'V']
    ['D';'T';'S';'V']
    ['J';'M';'B';'D';'T';'Z';'Q']
    ['L';'Z';'C';'D';'J';'T';'W';'M']
    ['J';'T';'G';'W';'M';'P';'L']
    ['H';'Q';'F';'B';'T';'M';'G';'N']
    ['W';'Q';'B';'P';'C';'G';'D';'R']
]

type Instruction = {
    count: int
    fromStack: int
    toStack: int
}

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let createInstructFromString (s: string) =
    match s with
    | Regex @"move ([0-9]+) from ([0-9]+) to ([0-9]+)" [countS; fromStackS; toStackS] ->
        {
            count = Int32.Parse countS
            fromStack = (Int32.Parse fromStackS) - 1
            toStack = (Int32.Parse toStackS) - 1
        }
    | _ -> failwith "unmatched instruction"

let rec processInstruction (curr: Stacks) (instruct: Instruction) =
    if instruct.count = 0 then curr
    else
        let fromStack = curr.[instruct.fromStack]
        let boxToMove, fromStackTail = fromStack.Head, fromStack.Tail
        let newToStack = boxToMove::curr.[instruct.toStack]
        let newCurr = curr |> List.mapi (fun ix x ->
            if ix = instruct.fromStack then fromStackTail
            elif ix = instruct.toStack then newToStack
            else x)
        processInstruction newCurr { instruct with count = (instruct.count-1) }
        // match fromStack with
        // | [] -> curr
        // | x::xs ->
        //     let boxToMove, fromStackTail = x, xs
        //     let newToStack = boxToMove::curr.[instruct.toStack]
        //     let newCurr = curr |> List.mapi (fun ix x ->
        //         if ix = instruct.fromStack then fromStackTail
        //         elif ix = instruct.toStack then newToStack
        //         else x)
        //     processInstruction newCurr { instruct with count = (instruct.count-1) }

let readTopLine (s: Stacks) =
    let chars = [for x in s -> x.Head]
    String.Join("", chars)

open Helpers

let day5Part1 =
    readMap 5 1 createInstructFromString
    |> Seq.fold processInstruction startingPos
    |> readTopLine

let processInstruction2 (curr: Stacks) (instruct: Instruction) =
    let fromStack = curr.[instruct.fromStack]
    let boxesToMove = fromStack |> List.take instruct.count
    let newFromStack = fromStack |> List.skip instruct.count
    let newToStack = boxesToMove::[curr.[instruct.toStack]] |> List.concat
    curr |> List.mapi (fun ix x ->
        if ix = instruct.fromStack then newFromStack
        elif ix = instruct.toStack then newToStack
        else x)

let day5Part2 =
    readMap 5 1 createInstructFromString
    |> Seq.fold processInstruction2 startingPos
    |> readTopLine
