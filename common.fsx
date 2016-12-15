module String = 

  let trim (s : string) = s.Trim()
  let isNullOrWhiteSpace (s : string) = System.String.IsNullOrWhiteSpace s
  let split c (s : string) = s.Split([|c|]) |> Seq.ofArray
  let ofChars : seq<char> -> string = Array.ofSeq >> fun x -> System.String(x)
  let charAt i (str : string) = str.ToCharArray().[i]
  let toBytes : string -> byte[] = System.Text.Encoding.ASCII.GetBytes
  let contains (sub : string) (str : string) = str.IndexOf(sub) >= 0 
  let rev (str : string) = str |> Seq.toList |> List.rev |> ofChars 

module Int = 
  let parse (s : string) = System.Int32.Parse(s)
  let tryParse( s : string) = match System.Int32.TryParse s with | (true, i) -> Some i | _ -> None
  let abs (i : int) = System.Math.Abs(i)

  let max (i : int) (j : int) = System.Math.Max(i,j)
  let min (i : int) (j : int) = System.Math.Min(i,j)

let toString a = a.ToString()

//let modulo s y = y % s
//let minus s y = y - s
//let log label a= 
//  printfn "%s %A" label a
//  a

let modulo (m : int) n =
    let mod' = n % m
    if sign mod' >= 0 then mod'
    else abs m + mod'

module Regex = 
  let tryMatch pattern str  = 
    let m = System.Text.RegularExpressions.Regex.Match(str, pattern)
    match m.Success with
    | true -> 
      let x::xs= [ for g in m.Groups -> g.Value ]
      Some xs
    | false -> None

  let getMatches pattern str  = 
    seq { for m in  System.Text.RegularExpressions.Regex.Matches(str, pattern) do 
            match m.Success with
            | true -> yield [ for g in m.Groups -> g.Value ]
            | false -> ()

    } |> Seq.toList

module List = 

  let permutations lst = 
    let rec innerPermutations l thisPermutation = 
      seq {
        if l |> List.isEmpty then yield thisPermutation
        else 
          for element in l do
            yield! innerPermutations (l |> List.filter((<>) element)) (element::thisPermutation)
      }
    innerPermutations lst []

  let rec pairs l =
      match l with
      | [] | [_] -> []
      | h :: t -> 
          [for x in t do
              yield h,x
              yield! pairs t]


open System
open System.Security.Cryptography
open System.Text

module Cryptography = 
  let md5 (stringData : string) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(stringData |> String.toBytes))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string


let z = [0;1;2;3;4] |> List.permutations |> List.ofSeq |> List.length
