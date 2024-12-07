open System
open System.IO

let example = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

type Line = int64 * int64 list

let split (splitter: string) (s : string) = s.Split(splitter, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

let parse (str : string) =
    str.Split("\n")
    |> Array.map (fun s ->
        let sp = split ":" s
        let testValue = int64 sp[0]
        let list = split " " sp[1] |> Array.map int64 |> List.ofArray
        testValue, list)

let rec isValid ((testValue, nums): Line) =
    match nums with
    | [] -> false
    | [n] -> testValue = n
    | a::b::rest ->
        if List.sum nums = testValue then
            true
        else
            isValid (testValue, (a * b)::rest ) || isValid (testValue, (a + b)::rest)

let solve (str: string) =
    parse str
    |> Array.filter isValid
    |> Array.sumBy fst

let input = File.ReadAllText "day7/input.txt"

solve example
solve input



