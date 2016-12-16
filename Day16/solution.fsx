#load "./../common.fsx"

open Common

let reverseAndMirror ( str : string ) = 
  str |> Seq.rev |> Seq.map(function |'0' -> '1' | '1' -> '0') |> Array.ofSeq |> System.String


let chunckMe size (s : 'a seq) = 
  let rec chunkMeInner (s : 'a seq) final = 
    match s |> Seq.isEmpty with
    | true -> final 
    | false ->
      let result = s |> Seq.take size |> Seq.toArray |> Seq.singleton
      chunkMeInner (s |> Seq.skip size) (final |> Seq.append result)
  chunkMeInner s Seq.empty |> Seq.rev

let rec expand limit (str : string) =
  match str.Length >= limit with
  | true ->  str.Substring(0, limit)
  | false -> 
    let expanded = sprintf "%s0%s" str (reverseAndMirror str)
    expand limit expanded

let checkSumCandidate (digits : string) = 
  digits
  |> chunckMe 2
  |> Seq.map(fun [|a;b|] ->  if a = b then '1' else '0')
  |> Array.ofSeq |> System.String

let rec checkSum (str : string) = 
  match str.Length % 2 with
  | 1 -> str
  | _ -> checkSum (checkSumCandidate str)

let partOne  = "11110010111001001" |> expand 272 |> checkSum |> Array.ofSeq |> System.String
let partTwo  = "11110010111001001" |> expand 35651584 |> checkSum |> Array.ofSeq |> System.String

let s = "ABCDEF" |> chunckMe 2 |> Seq.toList
