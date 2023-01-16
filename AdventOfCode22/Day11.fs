module AdventOfCode22.Day11

open System
open System.Text.RegularExpressions
open Helpers

type Monkey = {
    id: int
    inventory: uint64 list
    operation: uint64 -> uint64
    test: uint64 -> bool
    trueTarget: int
    falseTarget: int
    inspectionCount: uint64
}

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let inputToMonkeyDefs (xs: string list) =
    xs
    |> List.filter (fun s -> (String.IsNullOrEmpty s) = false)
    |> List.chunkBySize 6
    |> List.map (fun mStrings ->
        let [idStr; strt; op; tst; tru; flse ] = (mStrings |> List.map (fun s -> s.Trim ()))
        let id =
            match idStr with
            | Regex "Monkey ([0-9]+):" [id'] -> Int32.Parse id'
            | _ -> failwith "oh no"
        let start =
            match strt with
            | Regex "Starting items: ([0-9, ]+)" [items] -> items.Split(',') |> Array.map (fun x -> x.Trim () |> UInt64.Parse) |> Array.toList
            | _ -> failwith "oh no"
        let operation =
            match op with
            | Regex "Operation: new = (old)?([0-9]+)? ([-+*\/]{1}) (old)?([0-9]+)?" [fstOld; fstNum; operator; sndOld; sndNum] ->
                let numLeft = if fstNum = "" then 0UL else UInt64.Parse fstNum
                let numRight = if sndNum = "" then 0UL else UInt64.Parse sndNum
                match (fstOld, sndOld, operator) with
                | "old", "old", "+" -> function x -> x + x
                | "old", "old", "-" -> function x -> x - x
                | "old", "old", "*" -> function x -> x * x
                | "old", "old", "/" -> function x -> x / x
                | "", "", "+"       -> function x -> numLeft + numRight
                | "", "", "-"       -> function x -> numLeft - numRight
                | "", "", "*"       -> function x -> numLeft * numRight
                | "", "", "/"       -> function x -> numLeft / numRight
                | "old", "", "+"    -> function x -> x + numRight
                | "old", "", "-"    -> function x -> x - numRight
                | "old", "", "*"    -> function x -> x * numRight
                | "old", "", "/"    -> function x -> x / numRight
                | "", "old", "+"    -> function x -> numLeft + x
                | "", "old", "-"    -> function x -> numLeft - x
                | "", "old", "*"    -> function x -> numLeft * x
                | "", "old", "/"    -> function x -> numLeft / x
                | _                 -> failwith "oh no"
            | _ -> failwith "oh no"
        let test =
            match tst with
            | Regex "Test: divisible by ([0-9]+)" [divisor] -> function x -> x % (UInt64.Parse divisor) = 0UL
            | _ -> failwith "oh no"
        let ifTrue =
            match tru with
            | Regex "If true: throw to monkey ([0-9]+)" [mid] -> Int32.Parse mid
            | _ -> failwith "oh no"
        let ifFalse =
            match flse with
            | Regex "If false: throw to monkey ([0-9]+)" [mid] -> Int32.Parse mid
            | _ -> failwith "oh no"
        { id = id; inventory = start; operation = operation; test = test; trueTarget = ifTrue; falseTarget = ifFalse; inspectionCount = 0UL })

let divideBy3 = function (x: uint64) -> x / 3UL

let processInventory (inv: uint64 list) (op: uint64 -> uint64) =
    inv |> List.map (op >> divideBy3)

let processRound (monkeys: Monkey list) p =
    let ids = monkeys |> List.map (fun x -> x.id) |> List.sort
    ids
    |> List.fold (fun (acc: Monkey list) curr ->
        let actingMonkey = acc |> List.find (fun x -> x.id = curr)
        let monkeyFollowingInspections = {
            actingMonkey with inventory = p actingMonkey.inventory actingMonkey.operation; inspectionCount = actingMonkey.inspectionCount + (uint64)actingMonkey.inventory.Length
        }
        let invTargets =
            monkeyFollowingInspections.inventory
            |> List.map (fun x ->
                if monkeyFollowingInspections.test x then
                    (x, monkeyFollowingInspections.trueTarget)
                else
                    (x, monkeyFollowingInspections.falseTarget))
        acc
        |> List.map (fun m ->
            let matches = invTargets |> List.filter (fun (_, id) -> id = m.id) |> List.map fst
            if (m.id = monkeyFollowingInspections.id) then
                { monkeyFollowingInspections with inventory = [] }
            else { m with inventory = List.concat [ m.inventory; matches ] })) monkeys

let day11Part1 () =
    let monkeys =
        readMap 11 1 id
        |> Seq.toList
        |> inputToMonkeyDefs
    [ for i in 1..20 -> i ]
    |> List.fold (fun acc _ -> processRound acc processInventory) monkeys
    |> List.map (fun m -> m.inspectionCount)
    |> List.sortDescending
    |> List.take 2
    |> List.fold (fun acc curr -> acc * curr) 1UL

let processInventory2 (inv: uint64 list) (op: uint64 -> uint64) =
    inv |> List.map op

// not working, unsure why?
let day11Part2 () =
    let monkeys =
        readMap 11 0 id
        |> Seq.toList
        |> inputToMonkeyDefs
    [ for i in 1..10000 -> i ]
    |> List.fold (fun acc _ -> processRound acc processInventory2) monkeys
    |> List.map (fun m -> m.inspectionCount)
    |> List.sortDescending
    |> List.take 2
    |> List.fold (fun acc curr -> acc * curr) 1UL
