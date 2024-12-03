open System.IO
open System.Text.RegularExpressions
#r "nuget: Unquote"
open Swensen.Unquote

let example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let parse str =
    let m = Regex.Matches(str, "mul\((?<fst>\d+),(?<snd>\d+)\)")
    m |> Seq.map (fun m -> int m.Groups["fst"].Value, int m.Groups["snd"].Value)
    
let removeDisabled (str: string) =
    Regex.Replace(str, "don't\(\).*?(do\(\))", "")
    
let solve (input : string) =
    input
    |> parse
    |> Seq.sumBy (fun (a, b) -> a * b)
let solve2 (input : string) =
    input
    |> _.ReplaceLineEndings("")
    |> removeDisabled
    |> parse
    |> Seq.sumBy (fun (a, b) -> a * b)

let input = File.ReadAllText "day3/input.txt"

test <@ solve example = 161 @>
test <@ solve2 example2 = 48 @>

solve input
solve2 input