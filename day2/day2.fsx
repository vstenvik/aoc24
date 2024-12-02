#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
open System
open System.IO
open FsToolkit.ErrorHandling

let example = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""" |> (_.Split("\n"))

let parseLine (str : string) =
    str.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> List.ofArray

let isConsistent (ns: int list) =
    let a = ns.Head
    let b = ns.Item 1
    let decreasing = b < a
    ns |> Seq.windowed 2 |> Seq.forall (fun pair ->
        let first = pair.[0]
        let snd = pair.[1]
        snd < first = decreasing)

let safeDiff (ns: int list) =
    ns
    |> List.windowed 2
    |> List.forall (fun l ->
        let first = l.Head
        let snd = l.Item 1
        let diff = abs (first - snd)
        diff > 0 && diff <= 3)
    
let isSafe (ns: int list) =
    isConsistent ns && safeDiff ns

let solve (input : string array) =
    input
    |> Seq.map parseLine
    |> Seq.filter isSafe
    |> Seq.length
    
    
let input = File.ReadAllLines "day2/input.txt"
    
example |> solve
input |> solve
