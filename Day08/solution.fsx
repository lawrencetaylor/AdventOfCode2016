#load "./../common.fsx"

open Common
open System.Text.RegularExpressions

type Instruction = 
  | Colour of int*int
  | RotateRow of int*int
  | RotateColumn of int*int

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Instruction = 

  let private (|IsColor|_|) = 
    Regex.tryMatch "rect (\d*)x(\d*)"
    >> Option.map(fun [x;y] -> Int.parse x, Int.parse y)

  let private (|IsRotateRow|_|) = 
    Regex.tryMatch "rotate row y=(\d*) by (\d*)"
    >> Option.map(fun [x;y] -> Int.parse x, Int.parse y)
  let private (|IsRotateColumn|_|) = 
    Regex.tryMatch "rotate column x=(\d*) by (\d*)"
    >> Option.map(fun [x;y] -> Int.parse x, Int.parse y)

  let parse  = 
    function 
    | IsColor (x,y) -> Colour(x,y)
    | IsRotateRow (x,y) -> RotateRow(x,y)
    | IsRotateColumn (x,y) -> RotateColumn(x,y)
    | _ -> failwithf "Unable to parse instruction"

type Display(x : int,y : int) = 

  let maxColIndex = x-1
  let maxRowIndex = y-1
  let display = Array2D.init y x (fun _ _ -> false)

  let color cX cY = 
    seq { for i in [0..cY-1] do
            for j in [0..cX-1] do
              yield display.[i,j] <- true 
    }

  let rotateRow rY shift = 
    let row = Array.init x (fun i -> display.[rY,i])
    let shiftedRow = Array.init x (fun i -> row.[(i-shift + x) % x])
    seq { for i in [0..maxColIndex] do
            yield display.[rY, i] <- shiftedRow.[i]
    }

  let rotateCol rX shift = 
      let col = Array.init y (fun i -> display.[i,rX])
      let shiftedCol = Array.init y (fun i -> col.[(i-shift + y) % y])
      seq { for i in [0..maxRowIndex] do
              yield display.[i, rX] <- shiftedCol.[i]
      }

  let print () = 
    seq {
      yield printfn "Display"
      for i in [0..maxRowIndex] do
        for j in [0..maxColIndex] do
          yield (if display.[i, j] then '#' else '.') |> printf "%c"
        yield printf "%s" System.Environment.NewLine
      yield printf "%s" System.Environment.NewLine
    } 

  let litCount () = 
    seq { for i in [0..maxRowIndex] do
            for j in [0..maxColIndex] do
              yield display.[i,j] 
    } |> Seq.filter(id) |> Seq.length
    
  member this.Invoke instruction = 
    match instruction with
    | Colour (cX, cY) -> color cX cY
    | RotateRow (rY, shift) -> rotateRow rY shift
    | RotateColumn (rX, shift) -> rotateCol rX shift

  member this.Print = print
  member this.LitCount = litCount

let instructions = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input")  
  |> Seq.map(Instruction.parse)

let display = Display(50,6)
let y = 
  seq {
    for i in instructions do
      yield! display.Invoke i
  } |> Seq.iter id

let z = display.LitCount()
let p = display.Print() |> Seq.iter id


  
