#load "./../common.fsx"

open Common



type Element = Polonium | Thulium | Promethium | Cobalt | Ruthemium
and Component = Chip of Element | Generator of Element 
and FloorState = Component list  list

let isChip = function | Chip _ -> true | Generator  _-> false
let isGenerator = isChip >> not
let getElement = function | Chip e | Generator e -> e

let toFloorState = id

let init = 
  [ [ Chip Polonium; Chip Thulium ]
    [ Generator Polonium ]            
    [ Generator Thulium ]              
    []                                          
  ] |> toFloorState

let printFloor (componets : Component list) = 
  componets
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
  if (l |> List.length < 2) then true
  else 
    let chipElements = l |> List.filter(isChip) |> List.map(getElement) |> List.ofSeq 
    let generatorElements = l |> List.filter(isGenerator) |> List.map(getElement)  |> List.ofSeq 
    generatorElements |> List.isEmpty || (chipElements |> List.except  generatorElements |> List.isEmpty) 

let validLiftContents floorContents =
  floorContents
  |> List.pairs
  |> List.map(fun (a,b) -> [a;b])
  |> List.filter(isAllowed)
  |> List.append (floorContents |> List.map(fun a -> [a]))

let validFloorMoves (current : int) (liftPossibilities : Component list) (currentState : FloorState) = 
  current 
  |> getNextPossibleFloors
  |> List.map(fun f -> (f, currentState.[f] |> List.append liftPossibilities))
  |> List.filter(snd >> isAllowed)
  |> List.map(fst)

let executeMove 
  (currentFloor : int) 
  (targetFloor : int) 
  (liftContents : Component list) 
  (floors : FloorState) : FloorState = 

  floors |> List.mapi( fun i c -> 
    match i with
    | j when j = currentFloor -> c |> List.except liftContents
    | j when j = targetFloor -> c |> List.append liftContents
    | _ -> c)

let nextMoves (currentFloor : int) (floorContents : FloorState) visited = 
  floorContents.[currentFloor]
  |> validLiftContents
  |> Seq.collect(fun c -> validFloorMoves currentFloor c floorContents |> List.map(fun f -> (f, c)))
  |> Set.ofSeq
  |> Set.map(fun (newFloor, liftContents) -> (newFloor, executeMove currentFloor newFloor liftContents floorContents))
  |> fun states ->  states - (visited |> Set.ofSeq)
  
let isSafe (floorState : FloorState) = 
  floorState.[0] |> List.isEmpty
  && floorState.[1] |> List.isEmpty
  && floorState.[2] |> List.isEmpty

let permute  (state : FloorState) f = 
  state
  |> List.map(fun cp -> 
    cp
    |> List.map(fun c -> 
    match c with
    | Chip e -> Chip (f e)
    | Generator e -> Generator (f e)))

let testPermutations = 
  let source = 
    [ Polonium ; Thulium ; Promethium ; Cobalt ; Ruthemium]

  let sourceIndices = 
    source
    |> List.mapi(fun i e -> (e,i))
    |> Map.ofList

  source
  |> List.permutations
  |> Seq.toList
  |> List.map(fun l -> fun elem ->  l.[sourceIndices.[elem]])

let equivalentStates permutations (floor : int,state : FloorState) = 
  permutations |> List.map(permute state >> fun s -> (floor, s))

let depthM equivalence initialFloor initialState = 
  seq {
    let q = new System.Collections.Generic.Queue<_>()
    let visitedStates = new System.Collections.Generic.HashSet<_>()
    let visits = new System.Collections.Generic.List<_>()

    let enqueueState (pathLength : int) state  = 
      state
      |> equivalence
//      |> fun e -> e |> List.length |> log "adding"
//                  e
      |> List.iter(fun s -> visitedStates.Add s |> ignore)
      let (f, s ) = state
      //visitedStates.Count |> log "Visisted"
      q.Enqueue (f, s, pathLength)

    enqueueState 0 (initialFloor, initialState)
    while (q.Count <> 0) do
      let (thisFloor, thisState, pathLength) = q.Dequeue() //|> log "Dequeued"
      //yield (pathLength, q.Count, visitedStates.Count)
      if isSafe thisState then 
        yield (thisState, pathLength)

      nextMoves thisFloor thisState visitedStates 
      |> Seq.iter(enqueueState (pathLength + 1))

      //q.Count |> log "Queue length"
        

  }

//let test = 
//  [
//    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
//    "The second floor contains a hydrogen generator."
//    "The third floor contains a lithium generator."
//    "The fourth floor contains nothing relevant."
//  ]

let (eState , eFloor) = 
  ( [ [ Chip Thulium; Chip Polonium]
      [ Generator Polonium]
      [ Generator Thulium]
      []
    ] |> toFloorState, 0
  )

(*
The first floor contains 
  a polonium generator, 
  a thulium generator, 
  a thulium-compatible microchip, 
  a promethium generator, 
  a ruthenium generator, 
  a ruthenium-compatible microchip, 
  a cobalt generator, and a cobalt-compatible microchip.
The second floor contains 
  a polonium-compatible microchip and a promethium-compatible microchip.
The third floor contains nothing relevant.
The fourth floor contains nothing relevant.
*)

let (tState , tFloor) = 
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

let rec strictlyIncreasing lastMax s =
  let mutable lastMinimum = lastMax
  seq {
    for next in s do
      let (n,_,_) = next
      if (n > lastMinimum) then 
        yield next
        lastMinimum <-n
  }

let getFirst () = 
  depthM (equivalentStates testPermutations) eFloor eState 
  //|> strictlyIncreasing System.Int32.MinValue
  |> Seq.head// (printfn "Found: %A")

let ((_, length), ms) = (time getFirst) ()
printfn "Found path of length %i in %i ms" length ms



