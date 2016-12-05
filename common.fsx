module String = 

  let trim (s : string) = s.Trim()
  let isNullOrWhiteSpace (s : string) = System.String.IsNullOrWhiteSpace s
  let split c (s : string) = s.Split([|c|]) |> Seq.ofArray
  let ofChars : seq<char> -> string = Array.ofSeq >> System.String 
  let charAt i (str : string) = str.ToCharArray().[i]
  let toBytes : string -> byte[] = System.Text.Encoding.ASCII.GetBytes
  let contains (sub : string) (str : string) = str.IndexOf(sub) >= 0 



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

type State<'a, 's> = 'a*'s

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State = 
    let map f (a, b) = (f a, b)