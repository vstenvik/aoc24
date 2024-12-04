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


let xmas = Array.ofSeq "XMAS"
let xmasR = Array.rev xmas
let isXmas (chars : char array) =
    chars = xmas || chars = xmasR
    
let getHorizontal (chars : char array array) (y: int,x: int) =
    if x + 4 > chars[0].Length || y >= chars.Length then
        [||]
    else
        [|0;1;2;3|] |> Array.map (fun n -> chars[y][x + n])
let getVertical (chars : char array array) (y: int,x: int) =
    if y + 4 > chars.Length || x >= chars[0].Length then
        [||]
    else
        [|0;1;2;3|] |> Array.map (fun n -> chars[y + n][x])
    
let getDiagonal (chars : char array array) (y: int,x: int) =
    if y + 4 > chars.Length || x + 4 > chars[0].Length then
        [||]
    else
        [|0;1;2;3|] |> Array.map (fun n -> chars[y + n][x + n])
        
let getDiagonalL (chars : char array array) (y: int,x: int) =
    if y + 4 > chars.Length || x - 3 < 0 then
        [||]
    else
        [|0;1;2;3|] |> Array.map (fun n -> chars[y + n][x + -n])

let rec countXmas (chars : char array array) (y: int, x: int) (sum: int) =
    if y >= chars.Length then
        sum
    else
        let h = if isXmas <| getHorizontal chars (y,x) then 1 else 0
        let v = if isXmas <| getVertical chars (y,x) then 1 else 0
        let d = if isXmas <| getDiagonal chars (y,x) then 1 else 0
        let dL = if isXmas <| getDiagonalL chars (y,x) then 1 else 0
        //printf $"{y}{{%d{x}}} {{%d{h}}}{{%d{v}}}{{%d{d}}}{{%d{dL}}}\n"
        let nextY = if x = chars[0].Length - 1 then y + 1 else y
        let nextX = (x + 1) % chars[0].Length
        countXmas chars (nextY, nextX) (sum + h + v + d + dL)
  
countXmas example (0,0) 0

let input = File.ReadAllLines "day4/input.txt" |> Array.map Array.ofSeq

countXmas input (0,0) 0

test <@ isXmas ("XMAS" |> Array.ofSeq ) = true @>
test <@ isXmas ("" |> Array.ofSeq ) = false @>
test <@ isXmas ("MXAS" |> Array.ofSeq ) = true @>
test <@ getHorizontal example (0,0) = [|'M';'M';'M';'S'|] @>
test <@ getHorizontal example (0,6) = [|'M';'A';'S';'M'|] @>
test <@ getHorizontal example (0,7) = [||] @>
test <@ getHorizontal example (9,0) = [|'M'; 'X'; 'M'; 'X'|] @>
test <@ getHorizontal example (10,0) = [||] @>
test <@ getVertical example (0,0) = [|'M'; 'M'; 'A'; 'M'|] @>
test <@ getDiagonal example (0,0) = [|'M'; 'S'; 'X'; 'M'|] @>


