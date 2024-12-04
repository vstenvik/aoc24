open System.IO
open System.Text.RegularExpressions
#r "nuget: Unquote"
open Swensen.Unquote

let example = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX""" |> _.Split("\n") |> Array.map Array.ofSeq


let mas = Array.ofSeq "MAS"
let masR = Array.rev mas
let isXmas (chars : char array) =
    chars = mas || chars = masR
    
let getDiagonal (chars : char array array) (y: int,x: int) =
    if (y - 1) < 0 || (y + 1) >= chars.Length || (x-1) < 0 || (x+1) >= chars[0].Length then
        [||]
    else
        [|-1;0;1|] |> Array.map (fun n -> chars[y + n][x + n])
        
let getDiagonalL (chars : char array array) (y: int,x: int) =
    if (y - 1) < 0 || (y + 1) >= chars.Length || (x-1) < 0 || (x+1) >= chars[0].Length then
        [||]
    else
        [|1;0;-1|] |> Array.map (fun n -> chars[y + n][x + -n])

let rec countXmas (chars : char array array) (y: int, x: int) (sum: int) =
    if y >= chars.Length then
        sum
    else
        let d = isXmas <| getDiagonal chars (y,x)
        let dL = isXmas <| getDiagonalL chars (y,x)
        let nextY = if x = chars[0].Length - 1 then y + 1 else y
        let nextX = (x + 1) % chars[0].Length
        if d && dL then
            countXmas chars (nextY, nextX) (sum + 1)
        else
            countXmas chars (nextY, nextX) (sum)
  
countXmas example (0,0) 0

let input = File.ReadAllLines "day4/input.txt" |> Array.map Array.ofSeq

countXmas input (0,0) 0

test <@ isXmas ("XMAS" |> Array.ofSeq ) = true @>
test <@ isXmas ("" |> Array.ofSeq ) = false @>
test <@ isXmas ("MXAS" |> Array.ofSeq ) = true @>