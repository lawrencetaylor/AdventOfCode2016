#load "./../common.fsx"
open Common

type Node = 
  {
    Position : int*int
    Used : int
    Avail : int
    Use : int
    Size : int
  }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Parsing = 

  let position = function
    | Regex "\/dev\/grid\/node-x(\d+)-y(\d+)" [x;y] -> (int x, int y)
    | s -> failwithf "Failed to match position with %s" s

  let space  = function
    | Regex "(\d+)T" [s] -> (int s)
    | s -> failwithf "Failed to match space with %s" s

  let usage = function 
    | Regex "(\d+)%" [s] -> (int s)
    | s -> failwithf "Failed to match usage with %s" s


  let parse (str : string) = 
    let cols = 
      str 
      |> String.split ' ' 
      |> Seq.map(String.trim) 
      |> Seq.filter(String.isNullOrWhiteSpace >> not) |> List.ofSeq
    { Position = position cols.[0]
      Size = space cols.[1]
      Used = space cols.[2]
      Avail = space cols.[3]
      Use = usage cols.[4]
    }

let areCompatible (a, b) = 
  if a.Used = 0 then false 
  else if a = b then false
  else a.Used <= b.Avail

let input = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input")
  |> Array.toList
  |> List.skip 2
  |> List.map Parsing.parse

let testInput = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/testinput")
  |> Array.toList
  |> List.skip 2
  |> List.map Parsing.parse

let print currentPosition input = 
  let highestX, highestY = input |> Map.toSeq |> Seq.map fst |> Seq.max 
  printfn "\nMAZE\n"
  seq {
    for j in [0..highestY] do
      for i in [0..highestX] do
        yield printf "%s"
          ( if (i,j) = currentPosition then "_"
            else if i = highestX && j = 0 then "G"
            else if not input.[(i,j)] then "#"
            else ".")
      yield printf "\n"
  } |> Seq.iter id

let toMaze input = 
  let positionMap = input |> Seq.map(fun i -> i.Position, i) |> Map.ofSeq
  let highestX, highestY = input |> Seq.map(fun i -> i.Position) |> Seq.max 
  seq {
    for j in [0..highestY] do
      for i in [0..highestX] do
        let thisPosition = input |> Seq.find(fun p -> p.Position = (i,j))
        let (x,y) = thisPosition.Position
        yield ((x,y), (x,y) = (highestX-1, 0) || thisPosition.Use < 90)
  } |> Map.ofSeq

let startingPosition = 
  Seq.filter(fun n -> n.Used = 0)
  >> Seq.exactlyOne

let shortestPath (maze : Map<_,_>) (start : int*int) (finish : int*int) = 
  let q = System.Collections.Generic.Queue<_>()
  let v = System.Collections.Generic.HashSet<_>()
  q.Enqueue (start, 0)
  v.Add start |> ignore

  seq {
    while q.Count > 0 do
      let ((x,y), l) = q.Dequeue()

      if (x,y) = finish then 
        print (x,y) maze
        yield l
      else
        
        [(x+1, y); (x-1, y); (x,y+1); (x, y-1)]
        |> List.filter(maze.ContainsKey)
        |> List.filter(v.Contains >> not)
        |> List.filter(fun p -> maze.[p])
        |> List.iter(fun next -> 
          q.Enqueue (next, l+1)
          v.Add next |> ignore)
  } |> Seq.head

let cycle freeSpace goalSpace =
  let (gX, 0) = goalSpace 
  seq {
    // (position of free space, position of goal)
    yield (goalSpace, freeSpace)
    yield ((gX, 1), freeSpace)
    yield ((gX - 1, 1), freeSpace)
    yield ((gX - 2, 1), freeSpace)
    yield ((gX-2, 0), freeSpace)

  }

let rec cycleToPosition initFree initGoal = 
  seq {
    let thisCycle = cycle initFree initGoal |> List.ofSeq
    yield! thisCycle
    let (free, goal) = thisCycle |> Seq.last
    yield! cycleToPosition free goal
  }

let partOne = 
  input
  |> List.allPairs
  |> Seq.filter areCompatible
  |> Seq.length


let partTwo = 
  let { Position = s} = startingPosition input
  let (maxX, maxY) = input |> Seq.map(fun n -> n.Position) |> Seq.max
  let f = (maxX - 1, 0)
  let maze = toMaze input
  let toLeftOfGoal = shortestPath maze s f // BFS to move free space to left of Goal space
  let toCycleToOrigin = // Now to move the goal to the origin
    cycleToPosition (maxX - 1, 0) (maxX, 0) 
    |> Seq.map snd
    |> Seq.takeWhile ((<>) (0,0))
    |> Seq.length
  toLeftOfGoal + toCycleToOrigin + 1 // Seq.takeWhile will NOT include the move that takes G to (0,0)


  
      
