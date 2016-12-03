#load "../common.fsx"

open Common

type Cmd = U | R | D | L

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cmd = 
  let parse = function | 'U' -> U | 'D' -> D | 'R' -> R | 'L' -> L

type Position = int*int

let move (x,y) =
  function 
  | U -> (x, y - 1)
  | R -> (x + 1, y)
  | D -> (x, y + 1)
  | L -> (x - 1, y)

module PartOne = 
  let private keyPad = 
    [
      [1;2;3]
      [4;5;6]
      [7;8;9]
    ]

  let findKey (x,y) = keyPad.[y].[x].ToString()

  let boundsCheck (x, y) = 
    x >= 0
    && x <= 2
    && y >=0
    && y <= 2

module PartTwo = 

  let boundsCheck (x, y) = 
    y <= x + 2
    && y <= -x + 6
    && y >= x - 2
    && y >= -x + 2

  let private keyPad = 
    [
      [ 'X'; 'X'; '1'; 'X'; 'X']
      [ 'X'; '2'; '3'; '4'; 'X']
      [ '5'; '6'; '7'; '8'; '9']
      [ 'X'; 'A'; 'B'; 'C'; 'X']
      [ 'X'; 'X'; 'D'; 'X'; 'X']
    ]


  let findKey (x,y) = keyPad.[y].[x].ToString()

// If position after move is valid then chose that one, otherwise fall back to original position
let nextPosition isInBounds position (c : Cmd) = 
  let candidatePosition = move position c
  match candidatePosition |> isInBounds with
  | true -> candidatePosition 
  | false -> position

(*
  Gets the next position based one
  * A bounds checking function
  * An initial position
*)
let getNext boundsCheck initialPosition = Seq.map(Cmd.parse) >> Seq.fold (nextPosition boundsCheck) initialPosition

(*
  Get the code based on
  * A bounds checking function
  * A method to define the "Key" from a co-ordinate (x,y)
  * An initial position
  * A sequence of commands 
*)
let getCode boundsCheck findKey start commands = 
    let rec code lastPosition (codes : string seq)  =
      seq {
        match codes |> Seq.tryHead with
        | Some head -> 
          let nextPosition = head |> getNext boundsCheck lastPosition
          yield nextPosition |> findKey
          yield! code nextPosition (codes |> Seq.tail) 
        | None -> ()
      }
    code start commands

let getCodePartOne = getCode PartOne.boundsCheck PartOne.findKey
let getCodePartTwo = getCode PartTwo.boundsCheck PartTwo.findKey

let instructions = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input")
  |> Seq.filter(String.isNullOrWhiteSpace >> not)
let partOne = instructions |> getCodePartOne (1,1) |> Seq.toArray  |> String.concat ""
let partTwo = instructions |> getCodePartTwo (0,2) |> Seq.toArray  |> String.concat ""
