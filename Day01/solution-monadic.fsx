#load "../common.fsx"
#load "./solution.fsx"

open Common
open Solution

(*
The problem with my previous soution to this problem was that to solve the second part I had to pollute my code for the first part.
I had to carry about this sequence of points on the gird I had already visited - something which was not needed for Part 1.

The fundamentals of the problem is managing the navigation around the grid in response to instructions.
The core logic of this is defined in the "execute" function:
*)
     
let walk move state = 
  let distance = move |> Instruction.distance
  let (x,y) = state.Position
  match state.Direction with
  | North -> { state with Position = (x, y + distance)}
  | South -> { state with Position = (x, y - distance)}
  | East ->  { state with Position = (x + distance, y)}
  | West ->  { state with Position = (x - distance, y)}

// Instruction -> MyStep -> MyStep
let execute i = turn i >> walk i

(*
  The complexity in Part 2 is that we need to carry around some extra baggage.
  Because the signature of "execute" doesn't give us everything we need, we need to attach 
  some "side effect" (in the form of a tuple) to the output of this computation.

  In the specific case of Part 2, this state is the sequence of points that I am visiting.  
  In part 1, we can consider the degenerate case when the side effect is `unit`.
*)
type StateComputation<'a> = Computation of (MyState -> MyState*'a)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StateComputation = 

  let run computation state = 
    let (Computation c) = computation
    c state

  let combine (c : 'a -> 'a -> 'a) ( Computation f : StateComputation<'a>) ( Computation g : StateComputation<'a>) = 
    Computation (
      fun s -> 
        let (s', x) = f s
        let (s'', y) = g s'
        (s'', c x y)
      )

// In Part 1, we use 'unit' for the side effect
let movePartOne instruction = Computation(fun s -> (execute instruction s, ()) )

// In Part 2, we use 'seq<int*int>' for the side effect
let movePartTwo instruction = Computation(fun s -> 
  let { Position = (x,y); Direction = d} = s
  let newState = execute instruction s
  let distance = Instruction.distance instruction

  (newState, 
    match newState.Direction with
    | North -> seq { for i in 1..distance do yield  (x, y + i) }
    | East -> seq { for i in 1..distance do yield   (x + i, y) }
    | South -> seq { for i in 1..distance do yield  (x, y - i) }
    | West -> seq { for i in 1..distance do yield   (x - i, y) } )
  )
let instructions = 
  System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input").Split([|','|])
  |> Seq.map(String.trim >> Instruction.parse)

(*
  What's interesting, is that in both parts, the StateComputation functor 
  can be given a monoidal structure, e.g. I can go

    (Part 1) simply by chaining the "moves" with the 'unit' side effect.
    ( StateComputation<unit> , StateComputation<unit> ) -> StateComputation<unit>      

    (Part 2) by chaining the moves an concatentating the sequences.     
    ( StateComputation<seq<'a>> , StateComputation<seq<'a>> ) -> StateComputation<seq<'a>>  

  I *believe* this is translated as saying that the StateComputation functor (which is actually a monad), is a Transversable Functor.

  We use this monoidal structure to reduce a sequence of StateComputation<_> to a single StateComputation<_>, 
  which we then run using our starting point as it's parameter.
*)

let partOne = 
  instructions
  |> Seq.map(movePartOne)
  |> Seq.reduce(StateComputation.combine (fun _ _ -> ())) // effectively saying () + () = ()
  |> fun s -> StateComputation.run s { Position = (0,0) ; Direction = North}
  |> fun (x, _) -> x.Position |> minMoves

let partTwo = 
  instructions
  |> Seq.map(movePartTwo)
  |> Seq.reduce(StateComputation.combine Seq.append) 
  |> fun s -> StateComputation.run s { Position = (0,0) ; Direction = North}
  |> snd
  |> firstDuplicate
  |> minMoves


    






  
