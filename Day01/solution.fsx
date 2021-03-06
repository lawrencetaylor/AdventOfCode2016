#load "../common.fsx"

open Common
open System.IO

type Instruction = 
  | Left of int
  | Right of int

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Instruction = 
  let distance move = match move with | Left d | Right d -> d

  let parse (s : string) = 
    let parseCharArray = Array.ofList >> Seq.map(toString) >> String.concat "" >> Int.parse
    match s |> Seq.toList with 
    | 'L' :: xs -> xs |> parseCharArray |> Left
    | 'R' :: xs -> xs |> parseCharArray |> Right

type MyState = { Position : Position ; Direction : Direction }
and Position = int*int
and Direction = North | West | South | East

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Direction = 

  let turnLeft = function
    | North -> West
    | East -> North
    | South -> East
    | West -> South

  let turnRight = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North
  
  let turn = function
    | Left _ -> turnLeft
    | Right _ ->  turnRight
        
let walk move (state, prePath) = 
  let distance = move |> Instruction.distance
  let (x,y) = state.Position
  match state.Direction with
  | North -> ({ state with Position = (x, y + distance)}, Seq.append prePath (seq { for i in 1..distance do yield (x, y + i)})) 
  | South -> ({ state with Position = (x, y - distance)}, Seq.append prePath (seq { for i in 1..distance do yield (x, y - i)}))
  | East ->  ({ state with Position = (x + distance, y)}, Seq.append prePath (seq { for i in 1..distance do yield (x + i, y)}))
  | West ->  ({ state with Position = (x - distance, y)}, Seq.append prePath (seq { for i in 1..distance do yield (x - i, y)}))

let turn move state = { state with Direction = state.Direction |> Direction.turn move}
let moveFold state move = state |> State.map(turn move) |> walk move

let instructions = 
  File.ReadAllText(__SOURCE_DIRECTORY__ + "/input").Split([|','|])
  |> Seq.map(String.trim)
  |> Seq.map(Instruction.parse)

let initial = ({ Position = (0,0); Direction = North}, Seq.empty)

let minMoves (x, y) = (Int.abs x) + (Int.abs y)

let partOne = 
  instructions
  |> Seq.fold(moveFold) initial
  |> fst
  |> fun s -> minMoves s.Position
let firstDuplicate sequence =     
  let rec firstDupInner seq visited = 
    let head = seq |> Seq.head
    match visited |> Set.contains head with 
    | true -> head
    | false -> firstDupInner (Seq.tail seq) (visited |> Set.add head)
  firstDupInner sequence Set.empty

let partTwo = 
  instructions
  |> Seq.fold(moveFold) ({ Position = (0,0); Direction = North}, Seq.empty)
  |> snd
  |> firstDuplicate
  |> minMoves
