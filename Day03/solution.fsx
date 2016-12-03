#load "./../common.fsx"

open Common

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Candidate =

  let parse (str: string) = 
    str.Trim().Split([|' '|]) 
    |> Array.filter(String.isNullOrWhiteSpace >> not)
    |> Array.map(String.trim >> System.Convert.ToInt32)
    |> List.ofArray

  let isTriangle pointsList =
    let [g;y;z] = pointsList |> List.sortByDescending(id)
    g < y + z

let partOne = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input")
  |> Seq.filter(String.isNullOrWhiteSpace >> not)
  |> Seq.map(Candidate.parse)
  |> Seq.filter(Candidate.isTriangle)
  |> Seq.length

let flatten (s : seq<int list>) =
  seq {
    for i in s do 
      yield i.[0]
    for i in s do
      yield i.[1]
    for i in s do
      yield i.[2]
  }



let partTwo = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input")
  |> Seq.map(Candidate.parse)
  |> flatten
  |> Seq.chunkBySize 3
  |> Seq.filter(List.ofArray >>  Candidate.isTriangle)
  |> Seq.length
