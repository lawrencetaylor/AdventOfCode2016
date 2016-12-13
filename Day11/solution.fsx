#load "./../common.fsx"

open Common

/// Work in progress!

let test = 
  [
    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
    "The second floor contains a hydrogen generator."
    "The third floor contains a lithium generator."
    "The fourth floor contains nothing relevant."
  ]

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
  

let permute (state : FloorState) f = 
  state
  |> List.map(fun cp -> 
    cp
    |> Set.map(fun c -> 
    match c with
    | Chip e -> Chip (f e)
    | Generator e -> Generator (f e)))

let equivalentStates (state : FloorState) = 
  let permutations = 
    [ id
      function | Lithium -> Hydrogen | Hydrogen -> Lithium
    ]
  permutations |> List.map(permute state)
  

let getNextPossibleFloors = function
  | 0 -> [1]
  | 1 -> [0;2]
  | 2 -> [1;3]
  | 3 -> [2]

module Samples = 

  let endMinus = 
    [
      ([ []
         []
         []
         [ Generator Hydrogen; Generator Lithium; Chip Hydrogen; Chip Lithium ]
       ] |> toFloorState, 3)
      
      ([ []
         []
         [ Chip Hydrogen; Chip Lithium ]
         [ Generator Hydrogen; Generator Lithium]
       ] |> toFloorState, 2)

      ([  []
          []
          [ Chip Hydrogen]
          [ Generator Hydrogen; Generator Lithium; Chip Lithium]
        ] |> toFloorState, 3)

      ([ []
         []
         [ Generator Hydrogen; Generator Lithium; Chip Hydrogen]
         [ Chip Lithium ]
       ] |> toFloorState, 2)

      // End Minus 4
      ( [ []
          []
          [ Chip Hydrogen; Chip Lithium ]
          [ Generator Hydrogen; Generator Lithium]
          
      
        ] |> toFloorState, 3
      )
    ]
    |> List.mapi(fun i c -> i, c)
    |> Map.ofList




let isDisallowed l = 
  let chipElements = l |> Seq.filter(isChip) |> Seq.map(getElement) |> Set.ofSeq 
  let generatorElements = l |> Seq.filter(isGenerator) |> Seq.map(getElement)  |> Set.ofSeq 
  let nonMatchedChips = chipElements - generatorElements
  let nonMatchedGenerators = generatorElements - chipElements
  nonMatchedChips.Count > 0 && generatorElements.Count > 0


(*
  Disallow a list of components if it contains a chip and a Generator of a different type
*)
//let isDisallowed list = 
//  list |> Set.exists(fun l -> 
//      l |> isChip 
//    && 
//      let (Chip e) = l
//      let generatorElements = list |> Set.filter(isGenerator) |> Set.map(getElement)
//      generatorElements |> Set.isEmpty |> not //|> log "Generators Empty"
//      &&
//      generatorElements |> Set.contains e |> not
//      ) 

let validLiftContents floorContents =
  floorContents
  |> Set.toList
  |> List.pairs
  |> List.map(fun (a,b) -> [a;b] |> Set.ofList)
  |> List.filter(fun can -> 
    let isallowed = can |> isDisallowed |> not
    //printfn "Allowed: %b, Lift Contents %A" isallowed can
    isallowed)
  |> Set.ofList // Add single elements
  |> fun s -> s + (floorContents |> Set.map(Set.singleton))

let validFloorMoves (current : int) (liftPossibilities : Set<Component>) (currentState : FloorState) = 
  current 
  |> getNextPossibleFloors
  |> List.map(fun f -> (f, currentState.[f] + liftPossibilities))
  |> List.filter(snd >> isDisallowed >> not)
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

let prioritiseMostEmptry (s : int*FloorState) = 
  s 
  |> snd
  |> Seq.filter(Set.isEmpty)
  |> Seq.length
  |> fun a -> -a

let rec makeValidMoves currentFloor currentState visitedStates = 
  seq {
    for (newFloor, newState) in (nextMoves currentFloor currentState) |> List.ofSeq |> List.sortBy prioritiseMostEmptry do
      if newState |> isSafe then 
          yield Some (newState::visitedStates)
      else 
        match (visitedStates |> List.collect(equivalentStates) |> List.contains newState) with
        | true -> yield None
        | false -> 
            yield! makeValidMoves newFloor newState (newState::visitedStates)
  }

let rec strictlyDecreasing lastMin s =
  let mutable lastMinimum = lastMin
  seq {
    for (Some next) in s do
      if (next < lastMinimum) then 
        yield next
        lastMinimum <-next
  }

let thisGo = 3
let nextGo = thisGo - 1

let (eState , eFloor) = Samples.endMinus.[thisGo]
let (exectedState, expectedFloor) = Samples.endMinus.[nextGo]
let z = nextMoves eFloor eState |> List.ofSeq

let isValid = z |> Seq.map snd |> Seq.contains exectedState
let seqMoves = 
  makeValidMoves eFloor eState [] |> Seq.filter(Option.isSome) |> Seq.map(Option.map(fun s -> s.Length )) 
  |> strictlyDecreasing System.Int32.MaxValue
  |> Seq.take 3 |> Seq.toList



// Disallowed if there is a generator for a chip that does not have it's generator
//let isDisallowed2 list = 
//  list |> Set.exists(fun l -> 
//      l |> isGenerator 
//    && 
//      let (Generator e) = l
//      list |> Set.filter(isChip) |> Set.map(getElement) |> Set.contains e |> not
//      ) 

//let y = isDisallowed3 ([Chip Hydrogen; Chip Lithium; Generator Hydrogen] |> Set.ofSeq)

//let seqMoves = makeValidMoves eFloor eState [] |> Seq.filter(Option.isSome) |> Seq.map(Option.map(fun s -> s.Length )) |> Seq.take 50 |> Seq.toList

// let y = 
//   seq {
//     for valid in (makeValidMoves eFloor eState [eState] |> Seq.filter(Option.isSome)) do
//       yield valid
//   } 
//   |> Seq.filter(Option.isSome)
//   |> Seq.take 100
// //  |> Seq.scan(fun lastMin (Some this) -> 
// //    if this < lastMin then this else lastMin) (System.Int32.MaxValue)
//   |> Seq.map(Option.map(List.iter(printState >> printfn "STATE\n %s")))
//   |> Seq.toArray

//let goodSequence = makeValidMoves 3 endMinus2 List.empty|> Seq.tryFind(Option.isSome)