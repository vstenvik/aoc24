#r "nuget: FsToolkit.ErrorHandling, 4.11.1"

open System
open System.IO

#r "nuget: Unquote"
open Swensen.Unquote

let example =
    """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""
    |> (_.Split("\n"))

let parseLine (str: string) =
    str.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> List.ofArray

let createCandidateVariations (ns: int list) =
    ns :: [ for i in 0 .. (ns.Length - 1) -> List.removeAt i ns ]

let hasConsistentDirection desc (a: int, b: int) = b < a = desc

let changeIsWithinSafeRange (a: int, b: int) =
    let diff = abs (a - b)
    diff > 0 && diff <= 3

let isSafe (ns: int list) =
    let isSafeList (o: int list) =
        let [ a; b ] = o
        let isDecreasing = b < a

        o
        |> List.pairwise
        |> List.forall (fun pair -> hasConsistentDirection isDecreasing pair && changeIsWithinSafeRange pair)

    createCandidateVariations ns |> List.tryFind isSafeList |> Option.isSome


let solve (input: string array) =
    input |> Seq.map parseLine |> Seq.filter isSafe |> Seq.length


let input = File.ReadAllLines "day2/input.txt"

example |> solve
input |> solve


test <@ "7 6 4 2 1" |> parseLine |> isSafe = true @>
test <@ "1 2 7 8 9" |> parseLine |> isSafe = false @>
test <@ "9 7 6 2 1" |> parseLine |> isSafe = false @>
test <@ "1 3 2 4 5" |> parseLine |> isSafe = true @>
test <@ "8 6 4 4 1" |> parseLine |> isSafe = true @>
test <@ "27 29 32 33 36 37 40 37" |> parseLine |> isSafe = true @>
test <@ "61 62 63 66 66" |> parseLine |> isSafe = true @>
test <@ "53 54 57 60 61 62 63 67" |> parseLine |> isSafe = true @>
test <@ "5 7 10 12 14 16 18 23" |> parseLine |> isSafe = true @>
test <@ "3 2 3 4 5" |> parseLine |> isSafe = true @>

