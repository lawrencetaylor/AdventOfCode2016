#load "./../common.fsx"

open Common

let isSpace d (a,b) = 
  (a+b)*(a+b) + 3*a + b + d
  |> fun i -> System.Convert.ToString(i, 2)
  |> Seq.filter((=) '1')
  |> Seq.length
  |> modulo 2
  |> (=) 0

let moves d (a,b) = 
  [(1,0) ; (0,1); (-1, 0); (0, -1)]
  |> List.map(fun (x,y) -> (x+a, y+b))
  |> List.filter(fun (x,y) -> x >= 0 && y >= 0)
  |> List.filter(isSpace d)

let rec find target d initial visited count = 
  seq {
    for move in moves d initial do
      if move = target then yield (count+1)
      else if visited |> Set.contains move then ()
      else yield! find target d move (visited |> Set.add move) (count+1)
  }

let rec strictlyDecreasing lastMin s =
  let mutable lastMinimum = lastMin
  seq {
    for next in s do
      if (next < lastMinimum) then 
        yield next
        lastMinimum <-next
  }

let partOne = 
  find (31,39) 1350 (1,1) ([(1,1)] |> Set.ofList) 0 
  |> strictlyDecreasing System.Int32.MaxValue
  |> Seq.take 6 // 94
  |> Seq.iter(printfn "Found: %i")

let rec explore limit d initial visited count = 
  seq {
    if count = limit then yield (visited)
    else 
      for move in moves d initial do
        if visited |> Set.contains move then yield visited
        else yield! explore limit d move (visited |> Set.add move) (count+1 )
  }

let partTwoTest = 
  explore 50 1350 (1,1) ([(1,1)] |> Set.ofList) 0 
  |> Seq.fold (+) Set.empty
  |> Set.count // 124