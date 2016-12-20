#load "./../common.fsx"
open Common
let parse = 
  String.split '-' 
  >> Seq.map(int64)
  >> Seq.toList
  >> (fun [a;b] -> (a,b))

let intervals = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input")
  |> Array.toList
  |> List.map(parse)
  |> List.filter(fun (a,b) -> (a <= b))
  |> List.sortBy fst

let rec findAllowed (maximum : int64) l = 
  seq {
    match l |> log "List" with
    | (a,b)::(c,d)::xs when b + 1L < c -> 
      for ip in [b+1L..c-1L] do
        yield ip
        yield! findAllowed maximum ((c,d)::xs)
    | (a,b)::(c,d)::xs -> yield! findAllowed maximum ((min a c, max b d)::xs)
    | [(a,b)] -> 
      for ip in [b+1L..maximum] do yield ip
    | [] -> ()
  }

let test = findAllowed 9L [(0L,2L); (4L,7L);(5L,8L)] |> Seq.toList

let partOne = findAllowed 4294967295L intervals |> Seq.head //31053880
let partTwo = findAllowed 4294967295L intervals |> Seq.length
