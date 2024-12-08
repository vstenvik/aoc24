open System
open System.IO
let example = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""

type Grid = char array array

let toGrid (str : string): Grid =
    str.Split("\n") |> Array.map Array.ofSeq

let findAntennas (grid: Grid) =
    seq {
        for i in 0..(grid.Length - 1) do
            for j in 0..(grid[0].Length - 1) do
                let element = grid[i][j]
                if element <> '.' then
                    yield (i,j), element
                
    }
    |> Seq.groupBy snd
    |> Map
    |> Map.map (fun _ v -> v |> Seq.map fst |> List.ofSeq)
    |> Map.toList
    |> List.map snd

let findAntinodesForPair (ya,xa) (yb,xb) =
    let dy = yb - ya
    let dx = xb - xa
    [(ya - dy, xa - dx); (ya + (2 * dy), xa + (2 * dx))]

let findAllAntinodes (antennas : (int * int) list) =
    let rec inner (state: Set<(int * int)> ) (lst : (int * int) list) =
        match lst with
        | [] | [_] -> state
        | a::rest ->
            let antinodes = rest |> List.collect (findAntinodesForPair a) |> Set
            inner (Set.union state antinodes) rest
    inner Set.empty antennas
    
let prnt (grid: Grid) (antinodes: Set<int * int>) =
    [ for i in 0..(grid.Length - 1) do
        [for j in 0..(grid[0].Length - 1) do
             if Set.contains (i,j) antinodes then
                 '#'
             else
                 grid[i][j]]]
    |> List.map (fun s -> String.Join("", s))
    |> fun s -> String.Join("\n", s)
    |> printfn "%A"

let sx = """....
..o.
.o..
...."""
let sx2 = """....
.o..
..o.
...."""

let isInGrid (grid : char array array) (y : int, x : int) =
    y >= 0 && y < grid.Length &&
    x >= 0 && x < grid[0].Length

let solve input =
    let grid = toGrid input
    let antennas = findAntennas grid
    antennas
    |> List.map findAllAntinodes
    |> Set.unionMany
    |> Set.filter (isInGrid grid)
    |> Set.count
    //|> prnt grid
    
let input = File.ReadAllText "day8/input.txt"
solve input 


