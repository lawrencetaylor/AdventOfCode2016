module String = 

  let trim (s : string) = s.Trim()
  let isNullOrWhiteSpace (s : string) = System.String.IsNullOrWhiteSpace s
  let split c (s : string) = s.Split([|c|]) |> Seq.ofArray
  let ofChars : seq<char> -> string = Array.ofSeq >> System.String 
  let charAt i (str : string) = str.ToCharArray().[i]
  let toBytes : string -> byte[] = System.Text.Encoding.ASCII.GetBytes
  let contains (sub : string) (str : string) = str.IndexOf(sub) >= 0 
  let rev (str : string) = str |> Seq.rev |> ofChars 



module Int = 
  let parse (s : string) = System.Int32.Parse(s)
  let tryParse( s : string) = match System.Int32.TryParse s with | (true, i) -> Some i | _ -> None
  let abs (i : int) = System.Math.Abs(i)

  let max (i : int) (j : int) = System.Math.Max(i,j)
  let min (i : int) (j : int) = System.Math.Min(i,j)

let toString a = a.ToString()

let modulo s y = y % s
let minus s y = y - s
let log label a= 
  printfn "%s %A" label a
  a

module Regex = 
  let tryMatch pattern str  = 
    let m = System.Text.RegularExpressions.Regex.Match(str, pattern)
    match m.Success with
    | true -> 
      let x::xs= [ for g in m.Groups -> g.Value ]
      Some xs
    | false -> None