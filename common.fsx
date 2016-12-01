module String = 

  let trim (s : string) = s.Trim()

module Int = 
  let parse (s : string) = System.Int32.Parse(s)
  let abs (i : int) = System.Math.Abs(i)

let toString a = a.ToString()

type State<'a, 's> = 'a*'s

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State = 
    let map f (a, b) = (f a, b)