module AdventOfCode22.Helpers

open System
open System.IO

let readInput day part =
    let baseDir = __SOURCE_DIRECTORY__
    let filePath = $"inputs/%d{day}-%d{part}.txt"
    let fullPath = Path.Combine(baseDir, filePath)
    File.ReadAllText(fullPath)

let toMappedLines (transformer: string -> 'a) (input: string) =
    input.Split(Environment.NewLine)
    |> Seq.map transformer

let readMap day part mapper =
    let baseDir = __SOURCE_DIRECTORY__
    let filePath = $"inputs/%d{day}-%d{part}.txt"
    let fullPath = Path.Combine(baseDir, filePath)
    File.ReadAllLines(fullPath)
    |> Seq.map mapper
