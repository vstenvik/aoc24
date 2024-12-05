open System.IO

let example = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

let parse (str: string) =
    let split = str.Split("\n\n")
    let updates = split[1] |> _.Split("\n") |> Array.map (_.Split(",") >> Array.map int) |> Array.map List.ofArray |> List.ofArray
    let rules = split[0] |> _.Split("\n") |> Array.map _.Split("|") |> Array.map (fun a -> int <| Array.head a, int <| Array.item 1 a) |> Set
    updates, rules

let getMiddleValue (lst: int list) = lst |> List.item (lst.Length / 2)

type Update =
    | Presorted of int list
    | Uncanged of int list

let isSorted (rules: Set<_>) (update: int list) =
    let rec inner (state: int list) (next: int list) =
        match next with
        | [a] -> a::state |> List.rev
        | a::b::rest -> if Set.contains (a,b) rules
                        then inner (a::state) (b::rest)
                        else inner (b::state) (a::rest)
        | _ -> failwith "Empty"
        
    let sorted = inner [] update
    if sorted = update then Some update else None
    
let solve input =
    let updates, rules = parse input
    updates
    |> List.choose (isSorted rules)
    |> List.sumBy getMiddleValue

let input = File.ReadAllText "day5/input.txt"
solve input
