module AdventOfCode22.Day7

open System.IO

type Tree =
| Directory of Name: string * Children: Tree list
| File of FileName: string * Size: int64

type CdDest =
| Up
| Down of Dest: string
| Outermost

type ListOutputItem =
| Subdirectory of Name: string
| ListFile of Name: string * Size: int64

type Command =
| ChangeDir of Destination: CdDest
| Ls of ListOutputItem list

open System
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let processLsRes (s: string) =
    match s with
    | Regex @"([0-9]+) (.+)" [size; name] ->
        let sizeNum = Int64.Parse size
        ListFile (name, sizeNum)
    | Regex @"dir (.+)" [dirName] ->
        Subdirectory dirName
    | _ -> failwith $"cannot process list output: %s{s}"

let processCd (s: string) =
    match s with
    | Regex @"\$ cd \/" _        -> Outermost
    | Regex @"\$ cd \.\." _      -> Up
    | Regex @"\$ cd (.+)" [dest] -> Down dest
    | _ -> failwith $"cannot process cd command: %s{s}"

let rec processCommandList (curr: string) (tail: string list) acc =
    match tail with
    | [] -> acc
    | x::xs ->
        let currentCommand =
            match curr with
            | Regex "\$ ls" []-> Ls (tail |> List.takeWhile (fun x -> x.StartsWith("$") = false) |> List.map processLsRes)
            | Regex "\$ cd.+" [] -> ChangeDir (processCd curr)
            | _ -> failwith "unrecognised command"
        match currentCommand with
        | ChangeDir c -> processCommandList x xs ((ChangeDir c)::acc)
        | Ls items ->
            let newTail = tail |> List.skipWhile (fun x -> x.StartsWith("$") = false)
            let newAcc = ((Ls items)::acc)
            match newTail with
            | [] -> newAcc
            | y::ys -> processCommandList y ys ((Ls items)::acc)

let processInput (xs: string list) = processCommandList xs.Head xs.Tail [] |> List.rev

let getCommandText (cmd: Command) =
    match cmd with
    | ChangeDir d ->
        match d with
        | Up -> "Change dir up"
        | Down dest -> sprintf $"Change dir down to %s{dest}"
        | Outermost -> "Change dir to root"
    | Ls items ->
        let itemStrings =
            items
            |> List.map (fun x ->
                match x with
                | Subdirectory name -> sprintf $"SubDir: %s{name}"
                | ListFile (name, size) -> sprintf $"File: %s{name} (%d{size} bytes)"
            ) |> List.rev
        let finalLines = "END LIST"::itemStrings |> List.rev
        let startLine = "List command, results:"
        let lines = startLine::finalLines
        String.Join(Environment.NewLine, lines)

// This is a reversed representation of the current cursor in the tree
// eg. ["a"; "e"; "/"] for /e/a
type Position = string list

type Tracker = Tree * Command * Position

let updateTree tree atNode newChildren =
    let rec updateTreeHelper tree' nodes =
        match nodes with
        | [x] ->
            match tree' with
            | Directory (d, _) when d = x -> Directory (d, newChildren)
            | node -> node
        | x::xs ->
            match tree' with
            | File (a,b) -> File (a,b)
            | Directory (d, children) when d = x ->
                let newDirChildren = children |> List.map (fun c -> updateTreeHelper c xs)
                Directory (d, newDirChildren)
            | node -> node
        | _ -> failwith "what went wrong??"
    updateTreeHelper tree atNode


let rec commandsToTree (curr: Tracker) (commands: Command list) =
    let (tree, cmd, pos) = curr
    match commands with
    | [] ->
        match cmd with
        | ChangeDir _ -> tree
        | Ls items ->
            let children =
                items
                |> List.map (fun x ->
                    match x with
                    | Subdirectory name -> Directory (name, [])
                    | ListFile(name, size) -> File(name, size))
            let navigator = pos |> List.rev
            updateTree tree navigator children
    | x::xs ->
        match cmd with
        | ChangeDir d ->
            match d with
            | Up -> commandsToTree (tree, x, pos.Tail) xs
            | Down dest -> commandsToTree (tree, x, dest::pos) xs
            | Outermost -> commandsToTree (tree, x, ["/"]) xs
        | Ls items ->
            let children =
                items
                |> List.map (fun x ->
                    match x with
                    | Subdirectory name -> Directory (name, [])
                    | ListFile(name, size) -> File(name, size))
            let navigator = pos |> List.rev
            let newTree = updateTree tree navigator children
            commandsToTree (newTree, x, pos) xs


let processCommandsToTree (commands: Command list) =
    let initialTree = Directory ("/", [])
    commandsToTree (initialTree, commands.Head, ["/"]) commands.Tail

// this is wrong.
let rec getDirectorySizes (tree: Tree) =
    match tree with
    | Directory (name, children) ->
        let sum = children |> List.fold (fun acc t ->
            acc + (getDirectorySizes t |> List.sumBy (fun (typ, _, x) -> if typ = "dir" then 0L else x))) 0L
        ("dir", name, sum)::List.collect getDirectorySizes children
    | File (name, size) -> [("file", name, size)]

open Helpers

// let debugPrint =
//     // 7-0 is the example, to verify results before testing on the "real" input
//     readMap 7 0 id
//     |> Seq.toList
//     |> processInput
//     |> List.map getCommandText
//     |> List.iter (fun s -> printfn $"%s{s}")

let sumDirSizes (xs: (string * string * int64) list) =
    xs
    |> List.filter (fun (x, _, _) -> x = "dir")
    |> List.map (fun (_, x, y) -> x,y)
    |> List.map (fun x ->
        let _, size = x
        size)
    |> List.filter (fun x -> x <= 100000)
    |> List.sum

let day7Part1 =
    let initialTree = Directory("/", [])
    let commands =
        readMap 7 1 id
        |> Seq.toList
        |> processInput
    commandsToTree (initialTree, commands.Head, ["/"]) commands.Tail
    |> getDirectorySizes
    |> sumDirSizes

let day7Part2 =
    let initialTree = Directory("/", [])
    let commands =
        readMap 7 1 id
        |> Seq.toList
        |> processInput
    let tree = commandsToTree (initialTree, commands.Head, ["/"]) commands.Tail
    let sizes = tree |> getDirectorySizes
    let (_, _, rootSize) = sizes |> List.find (fun (_, z, _) -> z = "/")
    let requiredSpace = 30000000L
    let remainingSpace = 70000000L - rootSize
    let atLeastSpace = requiredSpace - remainingSpace
    let (_, _, toDeleteSize) =
        sizes
        |> List.sortBy (fun (_, _, a) -> a)
        |> List.find (fun (_, _, x) -> x >= atLeastSpace)
    toDeleteSize
