#load "./../common.fsx"

open Common

module Marker = 

  type T = { Repetitions : int;  AffectedLength : int; Length : int }

  let extract xs = 
    let endOfMarkerIndex = xs |> List.findIndex((=) ')')
    let markerString = xs.[0..endOfMarkerIndex - 1] |> String.ofChars
    let [length; repititions] = markerString.Split([|'x'|]) |> List.ofArray |> List.map(Int.parse)
    { Repetitions = repititions; AffectedLength = length; Length = markerString.Length + 2}

type Part = PartOne | PartTwo

let rec decompress p  (running : System.Int64) (c : char list) = 
  match c with
  | '('::xs -> 
    let marker = Marker.extract xs 
    let newChars = xs |> List.skip (marker.Length  + marker.AffectedLength - 1)
    let innerFragment = xs.[marker.Length - 1..marker.Length + marker.AffectedLength - 2]

    let amountToAdd = 
      match p with
      | PartOne -> int64 marker.AffectedLength
      | PartTwo -> 
        match innerFragment with
        | [] -> 0L
        | _ -> decompress p  0L innerFragment

    let innerString = innerFragment
    decompress p  (running + (int64 marker.Repetitions)*amountToAdd) newChars
  | x::xs -> decompress p  (running + 1L) xs
  | [] -> 
      running
  
let partOne = 
  System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input")  
  |> String.trim
  |> Seq.toList
  |> decompress PartOne 0L

let partTwo = 
  System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input")  
  |> String.trim
  |> Seq.toList
  |> decompress PartTwo 0L
