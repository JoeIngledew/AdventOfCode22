module AdventOfCode22.Day4

open System
open Helpers

type Section(area: int seq, start: int, finish: int) =
    member this.Area = area
    member this.Start = start
    member this.Finish = finish
    member this.Contains (other: Section) =
        other.Start >= this.Start && other.Finish <= this.Finish
    member this.Overlaps (other: Section) =
        (other.Start >= this.Start && other.Start <= this.Finish)
        || (other.Finish <= this.Finish && other.Finish >= this.Finish)



let valueMap (s: string) =
    let sectionsArr =
        s.Split(',')
        |> Array.map (fun (x: string) ->
            let rangeTermsArr = x.Split('-')
            let start, finish = Int32.Parse rangeTermsArr.[0], Int32.Parse rangeTermsArr.[1]
            Section([start .. finish], start, finish))
    sectionsArr.[0], sectionsArr.[1]

let isContained (x: Section) (y: Section) =
    x.Contains(y) || y.Contains(x)

let day4Part1 =
    readMap 4 1 valueMap
    |> Seq.map (fun (x,y) -> isContained x y)
    |> Seq.filter id
    |> Seq.length

let overlaps (x: Section, y: Section) =
    x.Overlaps(y) || y.Overlaps(x)

let day4Part2 =
    readMap 4 1 valueMap
    |> Seq.map overlaps
    |> Seq.filter id
    |> Seq.length
