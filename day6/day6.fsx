#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
open System
open System.IO
open System.Threading
open FsToolkit.ErrorHandling
#r "nuget: Unquote"
open Swensen.Unquote

type MoveHistory = Set<(int * int) * char>
let prnt (grid: char array array) (moveHistory : MoveHistory) blockerPos =
    let history = Set.toList moveHistory |> Map
    let result = [ for i in 0..(grid.Length - 1) do
                      [ for j in 0..(grid[0].Length - 1 ) do
                          if (i,j) = blockerPos then 'O'
                          
                          else if Map.containsKey (i,j) history
                                           then Map.find (i,j) history
                          else grid[i][j] ] ]
                |> List.map (fun s -> String.Join("", s)) |> fun s -> String.Join("\n", s)
    
    Console.Clear()
    printf "%s\n\n" result

let example = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

let example2 = """..........
.........#
........#.
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

let parse (str: string) =
    str.Split("\n") |> Array.map Array.ofSeq

let isGuard = function
    | 'v' | '^' | '<' | '>' -> true
    | _ -> false
let getDirection = function
    | 'v' -> (1, 0)
    | '^' -> (-1, 0)
    | '<' -> (0, -1)
    | '>' -> (0, 1)
    | c -> failwithf "invalid guard direction %c" c
let turnGuard = function
    | 'v' -> '<'
    | '^' -> '>'
    | '<' -> '^'
    | '>' -> 'v'
    | c -> failwithf "invalid guard direction %c" c

let advance (guard : char) (y, x) =
    let (dy, dx) = getDirection guard
    (y + dy, x + dx)
    
let isInGrid (grid : char array array) (y : int, x : int) =
    y >= 0 && y < grid.Length &&
    x >= 0 && x < grid[0].Length


let getNextDirection = turnGuard >> getDirection

let rec getNextCell (grid : char array array) (guard : char) (pos: int * int) =
    let ny,nx = advance guard pos
    if (grid[ny][nx]) <> '#' then
        guard, (ny,nx)
    else
        getNextCell grid (turnGuard guard) pos
    
let findStartIndex (arr : char array array) =
    let y,x = Seq.tryPick id <| seq {
                    for i in 0..(arr.Length - 1) do
                        for j in 0..(arr[0].Length - 1) ->
                            if isGuard (arr[i][j]) then Some (i,j) else None
                }
                |> Option.get
    (y,x), arr[y][x]
    
let wouldATurnNowResultInLoop (grid: char array array) (guard: char) (prevMoves: MoveHistory) (pos: int * int) =
    let newGrid = grid |> Array.map Array.copy
    let _, (by,bx) = getNextCell newGrid guard pos
    newGrid[by][bx] <- '#'
    //prnt newGrid prevMoves (by,bx)
    //Thread.Sleep(2000)
    let h = Set.map fst prevMoves
    if Set.contains (by,bx) h then
        false
    else
        let rec move (history: Set<_>) (g: char) (p: int * int) =
            if Set.contains (p, g) history
            then
                //prnt newGrid (Set.union history prevMoves) (by,bx)
                //Thread.Sleep(4000)
                true
            else if advance g p |> isInGrid newGrid |> not then
                //prnt newGrid (Set.union history prevMoves) (by,bx)
                //Thread.Sleep(250)
                false
            else
                //prnt newGrid (Set.union history prevMoves) (by,bx)
                //Thread.Sleep(250)
                let newGuard, newPos = getNextCell newGrid g p
                move (Set.add (p, g) history) newGuard newPos
                
        let turnDir = turnGuard guard
        let newTurnDir, newPos = getNextCell newGrid turnDir pos
        move (prevMoves |> Set.add (pos, guard)) newTurnDir newPos

    
type State = {
    history: Set<(int * int) * char>
    blockers: Set<int * int>
}

let countMoves grid =
    let startIndex, guard = findStartIndex grid
    let rec move (state: State) (g: char) (pos : int * int) =
        if (advance g pos) |> isInGrid grid |> not then
            state
        else
            let newState = { state with
                                 history = state.history |> Set.add (pos, g)
                                 blockers = if wouldATurnNowResultInLoop grid g state.history pos
                                                then state.blockers |> Set.add (advance g pos)
                                                else state.blockers }
            let newGuard, newPos = getNextCell grid g pos
            move newState newGuard (newPos)
    move { history = [(startIndex, guard)] |> Set; blockers = Set.empty } guard (startIndex)
    
let input = File.ReadAllText "day6/input.txt"

let solve str =
    str
    |> parse
    |> countMoves
    |> fun s -> Set.count s.blockers

test <@ solve example = 6 @>

solve input
printf "Solving"
//solve example2
printf "Done"
