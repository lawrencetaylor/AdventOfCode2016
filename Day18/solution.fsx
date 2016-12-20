// Then, a new tile is a trap only in one of the following situations:

// Its left and center tiles are traps, but its right tile is not.
// Its center and right tiles are traps, but its left tile is not.
// Only its left tile is a trap.
// Only its right tile is a trap.

#load "./../common.fsx"
open Common

type Tile = Safe | Trap

let isTrap = function
  | (Trap, Trap, Safe)
  | (Safe, Trap, Trap) 
  | (Trap, Safe, Safe)
  | (Safe, Safe, Trap) -> Trap
  | _ -> Safe

let toTriples l = 
  let rec toTriplesInner ls triples = 
    match ls with 
    | a::b::c::xs -> toTriplesInner (b::c::xs) ((a,b,c)::triples)
    | [a;b] -> triples |> List.rev
  toTriplesInner (List.append (Safe::l) [Safe]) []

let nextRow = toTriples >> List.map(isTrap)

let parse (str : string) = 
  str |> Seq.map(function | '.' -> Safe | '^' -> Trap) |> Seq.toList

let getSafeTiles rowCount starting = 
  [1..rowCount - 1]
  |> Seq.fold(fun lastRows i -> 
    match lastRows with
    | lastRow::previous -> (nextRow lastRow)::lastRows) [starting |> parse ]
  |> List.collect(List.filter(function | Safe -> true | Trap -> false))
  |> List.length

let test = getSafeTiles 10 ".^^.^.^^^^"

let initial = ".^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^."

let partOne = getSafeTiles 40 initial
let partTwo = getSafeTiles 400000 initial


