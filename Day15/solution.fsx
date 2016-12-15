#load "./../common.fsx"

open Common

type Disc = Disc of int*int

let partOneProblem = 
  [ Disc(1,5) , 2
    Disc(2,13), 7
    Disc(3,17), 10
    Disc(4,3) , 2
    Disc(5,19), 9
    Disc(6,7) , 0
  ]

let partTwoProblem = partOneProblem |> List.append [ Disc(7,11), 0]

let satisfies problem = 
  let subProblems = 
    problem 
    |> List.map(fun (Disc (number, size), start) -> 
      let required =  (size - number - start) |> modulo size
      fun test ->  test |> modulo size  |> (=) required)
  fun test -> subProblems |> Seq.forall(fun t -> t test)

let partOne = 
  Seq.initInfinite id
  |> Seq.filter(satisfies partOneProblem)
  |> Seq.head

let partTwo = 
  Seq.initInfinite id
  |> Seq.filter(satisfies partTwoProblem)
  |> Seq.head
