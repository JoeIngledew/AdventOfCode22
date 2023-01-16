module AdventOfCode22.Day19

open System
open System.Text.RegularExpressions
open Helpers

type Blueprint = {
    Id: int32
    OreBotCost: int32
    ClayBotCost: int32
    ObsidiBotCost: int32 * int32
    GeodeBotCost: int32 * int32
}

let (|RegexNum|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> Int32.Parse g.Value ])
    else None

let bpPattern = @"Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore\. Each clay robot costs ([0-9]+) ore\. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay\. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian\."

let toBlueprint (s: string) =
    match s with
    | RegexNum bpPattern [id; oc; cc; obc1; obc2; gc1; gc2] ->
        { Id = id; OreBotCost = oc; ClayBotCost = cc; ObsidiBotCost = (obc1, obc2); GeodeBotCost = (gc1, gc2) }
    | _ -> failwith $"Unable to parse %s{s}"

type BpState = {
    OreBotCount: int32
    ClayBotCount: int32
    ObsidiBotCount: int32
    GeodeBotCount: int32
    OreStock: int32
    ClayStock: int32
    ObsidiStock: int32
    GeodesCracked: int32
    Minute: int8
}

let canMakeOreBot bp state = state.OreStock >= bp.OreBotCost
let canMakeClayBot bp state = state.OreStock >= bp.ClayBotCost
let canMakeObsidiBot bp state = state.OreStock >= fst bp.ObsidiBotCost && state.ClayStock >= snd bp.ObsidiBotCost
let canMakeGeodeBot bp state = state.OreStock >= fst bp.GeodeBotCost && state.ObsidiStock >= snd bp.GeodeBotCost

type MakeInstruct =
| OreBot
| ClayBot
| ObsidiBot
| GeodeBot
| Nothing

let advanceState i state =
    let newBotState =
        match i with
        | OreBot -> { state with OreBotCount = state.OreBotCount + 1 }
        | ClayBot -> { state with ClayBotCount = state.ClayBotCount + 1 }
        | ObsidiBot -> { state with ObsidiBotCount = state.ObsidiBotCount + 1 }
        | GeodeBot -> { state with GeodeBotCount = state.GeodeBotCount + 1 }
        | Nothing -> state
    let newOreStock = state.OreStock + state.OreBotCount
    let newClayStock = state.ClayStock + state.ClayBotCount
    let newObsidiStock = state.ObsidiStock + state.ObsidiBotCount
    let newGeodeStock = state.GeodesCracked + state.GeodeBotCount
    { newBotState with OreStock = newOreStock; ClayStock = newClayStock; ObsidiStock = newObsidiStock; GeodesCracked = newGeodeStock }

let getPossibilities bp state =
    let geodeAvail = canMakeGeodeBot bp state
    let obsidiAvail = canMakeObsidiBot bp state
    let clayAvail = canMakeClayBot bp state
    let oreAvail = canMakeOreBot bp state
    [Nothing;GeodeBot;ObsidiBot;ClayBot;OreBot]
    |> List.filter (fun x -> (geodeAvail || x <> GeodeBot) && (obsidiAvail || x <> ObsidiBot) && (clayAvail || x <> ClayBot) && (oreAvail || x <> OreBot))
