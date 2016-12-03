#load "../common.fsx"
#load "./solution.fsx"

open Common
open Solution

type S<'s, 'a> = State of ('s -> 's*'a)

let bind (f : 'a -> S<'s, 'b>) (State aF : S<'s, 'a>) = 
  State(fun s -> 
    let (s', a) = aF s
    let (State bF) = f a
    bF s') 

let run s (State f : S<'s, 'b>) = f s

let log label a = 
  printfn "%s: %A" label a
  a

type Journey = Direction seq
type Instructions = Instruction seq

let f : Instruction -> S<Direction, Journey> = 
  let p instruction = 
    State(fun d -> 
      let newDirection = d |> Direction.turn instruction
      let distance = instruction |> Instruction.distance
      (newDirection, seq{ for i in 1..distance do yield newDirection}))
  p

let combine (State fA : S<Direction, Journey>) ( State fB : S<Direction, Journey>) : S<Direction, Journey>= 
  State(fun d -> 
    let (d', j) = fA d
    let (d'', j') = fB d'
    (d'', Seq.append j j')
    )

let step = function 
  | North -> (0, 1)
  | East -> (1, 0)
  | South -> (0, -1)
  | West -> (-1, 0)

let add (a,b) (c, d) = (a + c, b + d)

let addFolder s (stepX, stepY) = 
    let (x, y) = Seq.head s
    let tail = Seq.tail s
    Seq.append [(x+stepX,y+stepY )] s

// Calculate journey from steps
let rec journey (sX,sY) steps = 
    seq {
        let (h1,h2) = Seq.head steps
        let t1 = Seq.tail steps
        yield! journey (sX+h1,sY+h2) t1
         
    }
    

let calculateJourney =
  Seq.map(f) 
  >> Seq.reduce combine 
  >> run North 
  >> snd 
  >> Seq.map step

let p1 = 
  instructions |> calculateJourney |> List.ofSeq //|> Seq.reduce add |> minMoves

let p2 = 
  instructions |> calculateJourney |> journey (0,0) |> List.ofSeq //Seq.reduce addFolder [(0,0)] //|> firstDuplicate// |> minMoves

//let p3 = 
//    [Right 1
//     Right 1
//     Left 2]
//     |> calculateJourney
//     |> journey (0,0)
//     |> List.ofSeq
    






  
