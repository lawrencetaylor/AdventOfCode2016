#load "./../common.fsx"

open Common

type Element = Polonium | Thulium | Promethium | Cobalt | Ruthemium | Elerium | Dilithium
and Component = Chip of Element | Generator of Element 
and FloorState = Set<Component>  list

let isChip = function | Chip _ -> true | Generator  _-> false
let isGenerator = isChip >> not
let getElement = function | Chip e | Generator e -> e

let toFloorState = List.map(Set.ofList)

let init = 
  [ [ Chip Polonium; Chip Thulium ]
    [ Generator Polonium ]            
    [ Generator Thulium ]              
    []                                          
  ] |> toFloorState
  
let getNextPossibleFloors = function
  | 0 -> [1]
  | 1 -> [0;2]
  | 2 -> [1;3]
  | 3 -> [2]

(*
  This is the all important step.  
  It is easy to see that states that are simply a permutation of the underlying elements will take the same number of attempts to finish.
  So what we are really calculating is paths through equivalence classes of states - not states themselves.
  An equivalence class of states consists of a state + all states that are a permutation of the elements.
  IF you know that a current state is valid, the value (currentFloor, for each floor [(number of chips, number of generators)]) is invariant under permuations.
  More over if I have two such states with the same value of the above tuple, I can find a permutation that maps one to the other.
  So this value represents a node of our tree, and it is this value we need to check for to filter out equivalent states.
*)
let toRepresentative (floor : int,floorState : FloorState) = 
  (floor, floorState |> List.map(fun f -> (f |> Set.filter isChip |> Set.count, f |> Set.filter isGenerator |> Set.count)))

let isAllowed l = 
  if (l |> Set.count < 2) then true
  else 
    let chipElements = l |> Set.filter(isChip) |> Set.map(getElement)
    let generatorElements = l |> Set.filter(isGenerator) |> Set.map(getElement)  
    generatorElements |> Set.isEmpty || (chipElements -  generatorElements |> Set.isEmpty) 

let validLiftContents floorContents =
  floorContents
  |> Set.toList
  |> List.pairs
  |> List.filter(fun (a,b) -> 
    match a, b with
    | Chip a, Chip b -> true
    | Chip a, Generator b -> a = b
    | Generator a, Chip b -> a = b
    | Generator a, Generator b -> true)
  |> List.map(fun (a, b) -> [a;b] |> Set.ofList)
  |> List.append (floorContents |> Set.map(Set.singleton) |> Set.toList)

let validFloorMoves (current : int) (liftPossibilities : Set<Component>) (currentState : FloorState) = 
  current 
  |> getNextPossibleFloors
  |> List.map(fun f -> (f, currentState.[f] + liftPossibilities))
  |> List.filter(snd >> isAllowed)
  |> List.map(fst)

let executeMove 
  (currentFloor : int) 
  (targetFloor : int) 
  (liftContents : Set<Component>) 
  (floors : FloorState) : FloorState = 

  floors |> List.mapi( fun i c -> 
    match i with
    | j when j = currentFloor -> c - liftContents
    | j when j = targetFloor -> c + liftContents
    | _ -> c)

let nextMoves (currentFloor : int) (floorContents : FloorState) (visited : System.Collections.Generic.HashSet<_>) = 
  floorContents.[currentFloor]
  |> validLiftContents
  |> Seq.collect(fun c -> validFloorMoves currentFloor c floorContents |> List.map(fun f -> (f, c)))
  |> Set.ofSeq
  |> Set.map(fun (newFloor, liftContents) -> (newFloor, executeMove currentFloor newFloor liftContents floorContents))
  |> Set.filter(fun s -> 
    let classRep = s |> toRepresentative
    (visited.Contains classRep) |> not )
  
let isSafe (floorState : FloorState) = 
  floorState.[0] |> Set.isEmpty
  && floorState.[1] |> Set.isEmpty
  && floorState.[2] |> Set.isEmpty

let depthM initialFloor initialState = 
  seq {
    let q = new System.Collections.Generic.Queue<_>()
    let visitedStates = new System.Collections.Generic.HashSet<_>()
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    let mutable lastMaxLength = 0

    let enqueueState (pathLength : int) state  = 
      state
      |> toRepresentative
      |> fun s -> visitedStates.Add s 
      |> ignore
      let (f, s ) = state
      q.Enqueue (f, s, pathLength)

    enqueueState 0 (initialFloor, initialState)
    while (q.Count <> 0) do
      let (thisFloor, thisState, pathLength) = q.Dequeue()

      if isSafe thisState then 
        yield (thisState, pathLength)

      nextMoves thisFloor thisState visitedStates 
      |> Seq.iter(enqueueState (pathLength + 1))

  }

let test = 
  ( [ [ Chip Thulium; Chip Polonium]
      [ Generator Polonium]
      [ Generator Thulium]
      []
    ] |> toFloorState, 0
  )

let partOneInitialState = 
  ( [ [ Generator Polonium; 
        Generator Thulium; 
        Chip Thulium; 
        Generator Promethium; 
        Generator Ruthemium
        Chip Ruthemium
        Generator Cobalt
        Chip Cobalt]
      [ Chip Polonium; Chip Promethium]
      []
      []
    ] |> toFloorState, 0
  )

let partTwoInitialState = 
  ( [ [ Generator Polonium; 
        Generator Thulium; 
        Chip Thulium; 
        Generator Promethium; 
        Generator Ruthemium
        Chip Ruthemium
        Generator Cobalt
        Chip Cobalt
        Generator Elerium
        Generator Dilithium
        Chip Elerium
        Chip Dilithium
        ]
      [ Chip Polonium; Chip Promethium]
      []
      []
    ] |> toFloorState, 0
  )

let getFirst (intialState, initialFloor) = depthM initialFloor intialState |> Seq.head |> snd

let partOne = getFirst partOneInitialState
let partTwo = getFirst partTwoInitialState
