#load "./../common.fsx"
open Common

module Maze = 

  open System.Collections.Generic

  type T = Map<Position,bool>*Map<Location,Position>
  and TS = (bool [] [])*Map<Location,Position>
  and Position = int*int
  and Location = char

  let parse (input : string list) : T = 
    input 
    |> List.mapi(fun r str -> str |> Seq.mapi(fun c ch -> (r, c), ch)) 
    |> Seq.collect id
    |> Seq.fold (fun (maze, locations) (position, char) -> 
      match char with
      | '#' -> (maze |> Map.add position false, locations)
      | '.' -> (maze |> Map.add position true, locations)
      | l ->   (maze |> Map.add position true, locations |> Map.add l position)
      ) (Map.empty, Map.empty)

  let parseToArray input = 
    let maze = 
      input
      |> List.toArray
      |> Array.mapi(fun r str -> str |> Seq.toArray |> Array.mapi(fun c ch -> 
        match ch with
        | '#' -> false
        | _ -> true))
    maze

  let nextMoves  (x,y) (visited : HashSet<_>) (state : bool [] []) = 
    [0,1; 1,0; 0, -1; -1, 0]
    |> List.map(fun (i,j) -> (i+x, j+y))
    |> List.filter(fun (i,j) -> i >= 0 && i < state.Length && j >=0 && j < state.[0].Length)
    |> List.filter(fun (p,q) -> state.[p].[q])
    |> List.filter(fun p -> visited.Contains p |> not)

  // Calculates least path between two positions
  let leastPath ( maze : bool [] []) (b : Position) (e : Position) = 
    let q = new Queue<_>()
    q.Enqueue((b, 0))
    let v = new HashSet<_>()
    let distance = 
      seq {
        while q.Count > 0 do
          let (pos, length) = q.Dequeue()
          match pos with
          | p when p = e -> yield length
          | p -> 
            maze
            |> nextMoves pos v
            |> List.iter (fun p -> 
              v.Add p |> ignore
              q.Enqueue (p, length+1))
      } 
      |> Seq.tryHead
    distance
    
open Maze

(*
  Calculates the minimal length of a given path by
  summing together the minimal path length between each
  leg of the path.
*)
let calculateLength ((maze, positions) : Maze.TS) path = 
  let rec calculateLengthInner ((maze, positions) : Maze.TS) path length = 
    match path with
    | a::b::xs -> 
      let (Some pathLength) = Maze.leastPath maze positions.[a] positions.[b]
      calculateLengthInner (maze, positions) (b::xs) (length + pathLength)
    | [_] -> length 
  calculateLengthInner (maze, positions) path 0 

let solve pathCreator ((maze, locations) : Maze.TS) = 
  
  (*
    Strategy is to calculate all permutations of visits to the prescribed locations.
    For each permutation (representing a path):
      Find the minimal length of this path to visit all the locations in order
    Find the minimal value accross all paths
  *)

  let allPaths = 
    locations 
    |> Map.toSeq
    |> Seq.map fst 
    |> List.ofSeq 
    |> List.except ['0']
    |> List.permutations
    |> List.ofSeq

  allPaths
  |> Seq.map (fun a -> calculateLength (maze, locations) (pathCreator a))
  |> Seq.min

let testState = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +  "./input-test")
  |> List.ofArray
  |> Maze.parse

let actualState = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +  "./input")
  |> List.ofArray
  |> Maze.parse

let actualStateArray = 
  let maze = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +  "./input")
    |> List.ofArray
    |> Maze.parseToArray
  (maze, actualState |> snd)
  

(*
  This takes a long time to run!  I imagine it's because the Map is copied everywhere.
  Potentially cold be sped up using an Array...
*)
let partOne = solve (fun a -> '0'::a) actualStateArray // 448
let partTwo = solve (fun a -> List.append ('0'::a) ['0']) actualStateArray // 672
