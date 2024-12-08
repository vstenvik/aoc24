open System
open System.IO
type Grid = char array array
let isInGrid (grid : char array array) (y : int, x : int) =
    y >= 0 && y < grid.Length &&
    x >= 0 && x < grid[0].Length

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

let findAntinodesForPairInGrid (grid: Grid) (ya,xa) (yb,xb) =
    let dy = yb - ya
    let dx = xb - xa
    let rec whileInBounds op lst (cy,cx) =
        let next = (op cy dy, op cx dx)
        if isInGrid grid next |> not then
            lst
        else
            whileInBounds op (next::lst) next
    whileInBounds (-) [ya,xa] (ya,xa) @ whileInBounds (+) [yb,xb] (yb,xb) 

let findAllAntinodes (grid: Grid) (antennas : (int * int) list) =
    let rec inner (state: Set<(int * int)> ) (lst : (int * int) list) =
        match lst with
        | [] | [_] -> state
        | a::rest ->
            let antinodes = rest |> List.collect (findAntinodesForPairInGrid grid a) |> Set
            inner (Set.union state antinodes) rest
    inner Set.empty antennas

let solve input =
    let grid = toGrid input
    let antennas = findAntennas grid
    antennas
    |> List.map (findAllAntinodes grid)
    |> Set.unionMany
    |> Set.filter (isInGrid grid)
    |> Set.count
    
let input = File.ReadAllText "day8/input.txt"
solve input