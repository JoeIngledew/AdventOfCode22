module AdventOfCode22.Day2

type RPSChoice =
| Rock
| Paper
| Scissors

type Result =
| Loss
| Draw
| Win

let mapRPS s =
    match s with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith "unexpected left input"

let valueMap (x: string) =
    let xs = x.Split(" ")
    if xs.Length < 2 then failwith "Incorrect input count"
    let left = mapRPS xs.[0]
    let right = mapRPS xs.[1]
    (left, right)

let getResult (l: RPSChoice) (r: RPSChoice) =
    match (l,r) with
    | x, y when x = y -> Draw
    | Rock, Paper -> Win
    | Paper, Scissors -> Win
    | Scissors, Rock -> Win
    | _ -> Loss

let choicePoints x =
    match x with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let resultPoints r =
    match r with
    | Loss -> 0
    | Draw -> 3
    | Win -> 6

let calcScoreOfGame (l: RPSChoice, r: RPSChoice) =
    resultPoints (getResult l r) + choicePoints r

open Helpers

let day2Part1 =
    readMap 2 1 valueMap
    |> Seq.map calcScoreOfGame
    |> Seq.sum

let mapRes s =
    match s with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "unexpected result"

let getScoreFromLeftAndRes (left: RPSChoice) (res: Result) =
    let myChoice =
        match (left, res) with
        | x, Draw -> x
        | Rock, Loss -> Scissors
        | Rock, Win -> Paper
        | Paper, Loss -> Rock
        | Paper, Win -> Scissors
        | Scissors, Loss -> Paper
        | Scissors, Win -> Rock
    calcScoreOfGame (left, myChoice)

let valueMap2 (x: string) =
    let xs = x.Split(" ")
    if xs.Length < 2 then failwith "Incorrect input count"
    let left = mapRPS xs.[0]
    let right = mapRes xs.[1]
    getScoreFromLeftAndRes left right

let day2Part2 =
    readMap 2 1 valueMap2
    |> Seq.sum
