#load "./../common.fsx"
open Common

type Instruction = 
  | SwapIndices of int*int
  | SwapCharacters of char*char
  | Rotate of Direction*int
  | RotateFromPosition of char
  | ReverseFrom of int*int
  | Move of int*int
and Direction = Left | Right

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Parsing = 

  //swap position 7 with position 1
  let (|SwapI|_|)  = 
    Regex.tryMatch "swap position (\d) with position (\d)"
    >> Option.map(List.map(Int.parse) >> log "here" >> fun [x;y] -> x,y)

  //swap letter e with letter d
  let (|SwapC|_|) = 
    Regex.tryMatch "swap letter ([a-z]) with letter ([a-z])"
    >> Option.map(fun [x;y] -> x.[0], y.[0])

  //rotate right 1 step
  let (|RotateR|_|) = 
    Regex.tryMatch "rotate right (\d)"
    >> Option.map(List.map(Int.parse) >> fun [x] -> x)

  let (|RotateL|_|) = 
    Regex.tryMatch "rotate left (\d)"
    >> Option.map(List.map(Int.parse) >> fun [x] -> x)

  // rotate based on position of letter b
  let (|RotatePos|_|) = 
    Regex.tryMatch "rotate based on position of letter ([a-z])"
    >> Option.map(fun [x] -> x.[0])

  //reverse positions 2 through 5
  let (|Reverse|_|) = 
    Regex.tryMatch "reverse positions (\d) through (\d)"
    >> Option.map(List.map(Int.parse) >> fun [x;y] -> (x,y))

  //move position 6 to position 5
  let (|Move|_|) = 
    Regex.tryMatch "move position (\d) to position (\d)"
    >> Option.map(List.map Int.parse >> fun [x;y] -> (x,y))

  let parse = function
  | SwapI (x,y) -> SwapIndices(x,y)
  | SwapC (a,b) -> SwapCharacters(a,b)
  | RotateR s -> Rotate (Right, s)
  | RotateL s -> Rotate (Left, s)
  | RotatePos c -> RotateFromPosition c
  | Reverse (x,y) -> ReverseFrom (x,y)
  | Move (x,y) -> Move(x,y)
  | x -> failwithf "Could not match %s" x

let rotate direction shift (str : string) = 
  let stringSize = str.Length
  let signedShift = match direction with | Right -> - shift | Left -> shift
  str |> Seq.mapi(fun i _ -> str.[i + signedShift |> modulo stringSize]) |> String.ofChars

let invoke (str : string) instruction  = 
  match instruction with
  | SwapIndices (x,y) -> 
    str |> Seq.mapi(fun i c -> 
      match i with 
      | j when j = x -> str.[y]
      | j when j = y -> str.[x]
      | _ -> c) |> String.ofChars
  | SwapCharacters (a,b) -> 
    str |> Seq.map(fun c -> 
      match c with 
      | k when k = a -> b
      | k when k = b -> a
      | _ -> c) |> String.ofChars
  | Rotate (direction, shift) -> rotate direction shift str
  | RotateFromPosition c -> 
    let shift = 1 + str.IndexOf c
    let finalShift = if (str.IndexOf c) < 4 then shift else shift + 1
    rotate Right finalShift str
  | ReverseFrom (x, y) -> 
    str |> Seq.mapi(fun i c -> 
      match i with 
      | j when x <= j && j <= y -> 
        str.[x + y - j]
      | _ -> c) |> String.ofChars
  | Move (x,y) -> 
    if x < y then
      str |> Seq.mapi(fun i c -> 
        match i with 
        | i when i < x -> c
        | i when i < y -> str.[i+1]
        | i when i = y -> str.[x]
        | i when i > y -> c) |> String.ofChars
    else 
      str |> Seq.mapi(fun i c -> 
        match i with 
        | i when i < y -> c
        | i when i = y -> str.[x]
        | i when i <= x -> str.[i-1]
        | i when i > x -> c) |> String.ofChars

(*

let test = 
  [ SwapIndices (4,0)
    SwapCharacters ('d', 'b')
    ReverseFrom (0,4)
    Rotate (Left, 1)
    Move(1,4)
    Move(3,0)
    RotateFromPosition 'b'
    RotateFromPosition 'd'
  ]

let testPartOne = test |> Seq.fold invoke "abcde"
*)

let instructions = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input")
  |> Array.toList
  |> List.map(Parsing.parse)

let p1 = instructions |> Seq.fold invoke "abcdefgh" //fdhbcgea

let p2 = 
  "fbgdceah"
  |> List.ofSeq
  |> List.permutations
  |> Seq.map(String.ofChars)
  |> Seq.filter(fun test -> instructions |> Seq.fold invoke test |> (=) "fbgdceah")
  |> Seq.head //egfbcadh


    

