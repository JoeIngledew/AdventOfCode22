// For more information see https://aka.ms/fsharp-console-apps

open System.Diagnostics
open AdventOfCode22

let sw = Stopwatch()
sw.Start ()
// open Day1
//
// day1Part2
// |> printfn "%d"

// open Day2
//
// day2Part2 |> printfn "%d"

// open Day3
// day3part2 |> printfn "%d"

// open Day4
// day4Part2 |> printfn "%d"

// open Day5
// day5Part2 |> printfn "%s"

// open Day6
// day6Part2 |> printfn "%d"

// open Day7
// day7Part2 |> printfn "%d"

// open Day8
// () |> day8Part2 |> printfn "%d"
//
// open Day9
// () |> day9Part1 |> printfn "%d"

// open Day10
// //() |> day10Part1 |> printfn "%d"
// () |> day10Part2

// open Day11
// () |> day11Part2 |> printfn "%d"

() |> Day20.day20Part1 |> printfn "%d"

sw.Stop ()
printfn $"Time taken: %d{sw.ElapsedMilliseconds}"
