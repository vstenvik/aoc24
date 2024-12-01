open System.IO
open System.Text.RegularExpressions

let example =
    """3   4
4   3
2   5
1   3
3   9
3   3""".Split("\n")

let distance (a, b) = abs (a - b)

let parseLine (line: string) =
    let m = Regex.Match(line, "^(?<fst>\\d+)\\s+(?<snd>\\d+)")
    let fst = m.Groups["fst"].Value
    let snd = m.Groups["snd"].Value
    [| int fst; int snd |]

let solve (input: string[]) =
    input
    |> Array.map parseLine
    |> Array.transpose
    |> Array.map Array.sort
    |> (fun ([| a; b |]) -> Array.zip a b)
    |> Array.map distance
    |> Array.sum

let solve2 (input: string[]) =
    let [| listA; listB |] = input |> Array.map parseLine |> Array.transpose
    let occurr = listB |> Seq.countBy id |> Map

    listA
    |> Seq.sumBy (fun n ->
        match occurr.TryFind n with
        | Some i -> n * i
        | None -> 0)

let input = File.ReadAllLines "day1/input.txt"

example |> solve
input |> solve
example |> solve2
input |> solve2
