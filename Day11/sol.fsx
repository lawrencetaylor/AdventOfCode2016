#load "./../common.fsx"

open Common

type Element = Hydrogen | Lithium
and Component = Chip of Element | Generator of Element 
and FloorState = Set<Component>  list

let isChip = function | Chip _ -> true | Generator  _-> false
let isGenerator = isChip >> not
let getElement = function | Chip e | Generator e -> e

let toFloorState = List.map(Set.ofList)

let init = 
  [ [ Chip Hydrogen; Chip Lithium ]
    [ Generator Hydrogen ]            
    [ Generator Lithium ]              
    []                                          
  ] |> toFloorState

let printFloor (componets : Set<Component>) = 
  componets
  |> Set.toList
  |> List.sort
  |> List.map(fun c -> 
    match c with
    | Chip Hydrogen -> "HM"
    | Chip Lithium -> "LM"
    | Generator Hydrogen -> "GH"
    | Generator Lithium -> "GL")
  |> String.concat ", "

let printState (floorState) = 
  floorState
  |> List.mapi(fun i c -> 
    sprintf "F%i: %s" (i+1) (printFloor c))
  |> String.concat System.Environment.NewLine
  
let getNextPossibleFloors = function
  | 0 -> [1]
  | 1 -> [0;2]
  | 2 -> [1;3]
  | 3 -> [2]


let isAllowed l = 
  let chipElements = l |> Seq.filter(isChip) |> Seq.map(getElement) |> Set.ofSeq 
  let generatorElements = l |> Seq.filter(isGenerator) |> Seq.map(getElement)  |> Set.ofSeq 
  generatorElements |> Seq.isEmpty || (chipElements -  generatorElements) |> Set.isEmpty 

let validLiftContents floorContents =
  floorContents
  |> Set.toList
  |> List.pairs
  |> List.map(fun (a,b) -> [a;b] |> Set.ofList)
  |> List.filter(isAllowed)
  |> Set.ofList // Add single elements
  |> fun s -> s + (floorContents |> Set.map(Set.singleton))

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

let nextMoves (currentFloor : int) (floorContents : FloorState) = 
  floorContents.[currentFloor]
  |> validLiftContents
  |> Seq.collect(fun c -> validFloorMoves currentFloor c floorContents |> List.map(fun f -> (f, c)))
  |> Seq.map(fun (newFloor, liftContents) -> (newFloor, executeMove currentFloor newFloor liftContents floorContents))
  
let isSafe (floorState : FloorState) = 
  floorState.[0] |> Set.isEmpty
  && floorState.[1] |> Set.isEmpty
  && floorState.[2] |> Set.isEmpty

//let rec makeValidMoves currentFloor currentState visitedStates = 
//  seq {
//    for (newFloor, newState) in (nextMoves currentFloor currentState) |> List.ofSeq  do
//      if newState |> isSafe then 
//          //newState |> printState |> log "STATE: "
//          yield Some (newState::visitedStates)
//      else 
//        match (visitedStates |> List.collect(equivalentStates) |> List.exists((=) newState)) with
//        | true -> yield None
//        | false -> 
//            yield! makeValidMoves newFloor newState (newState::visitedStates)
//  }

let permute  (state : FloorState) f = 
  state
  |> List.map(fun cp -> 
    cp
    |> Set.map(fun c -> 
    match c with
    | Chip e -> Chip (f e)
    | Generator e -> Generator (f e)))

let testPermutations = 
  [ id
    function | Lithium -> Hydrogen | Hydrogen -> Lithium
  ]

let equivalentStates permutations (floor : int,state : FloorState) = 

  permutations |> List.map(permute state >> fun s -> (floor, s))

let depthM equivalence initialFloor initialState = 
  seq {
    let q = new System.Collections.Generic.Queue<_>()
    let visitedStates = new System.Collections.Generic.HashSet<_>()
    let visits = new System.Collections.Generic.List<_>()
    q.Enqueue (initialFloor, initialState, 0)
    while (q.Count <> 0) do
      let (thisFloor, thisState, pathLength) = q.Dequeue()
      if isSafe thisState then 
        yield (thisState, pathLength)

      let next = nextMoves thisFloor thisState |> List.ofSeq 
      next
      |> Seq.iter(fun (newFloor, newState) ->
        (newFloor, newState)
        |> equivalence
        |> List.iter(fun s -> visitedStates.Add s |> ignore)
        //let added = visitedStates.Add (newFloor, newState)
        q.Enqueue (newFloor, newState, pathLength + 1))

  }

//let test = 
//  [
//    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
//    "The second floor contains a hydrogen generator."
//    "The third floor contains a lithium generator."
//    "The fourth floor contains nothing relevant."
//  ]

let (eState , eFloor) = 
  ( [ [ Chip Lithium; Chip Hydrogen]
      [ Generator Hydrogen]
      [ Generator Lithium]
      []
    ] |> toFloorState, 0
  )

let seqMoves = depthM (equivalentStates testPermutations) eFloor eState |> Seq.head

//The first floor 
contains a polonium generator, 
a thulium generator, 
a thulium-compatible microchip, 
a promethium generator, 
a ruthenium generator, 
a ruthenium-compatible microchip, 
a cobalt generator, and a cobalt-compatible microchip.
//The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
//The third floor contains nothing relevant.
//The fourth floor contains nothing relevant.

polonium
thulium
promethium
cobalt
ruthenium


