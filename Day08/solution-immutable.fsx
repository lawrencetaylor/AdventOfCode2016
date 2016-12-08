#load "./solution.fsx"

open Solution

type Grid = bool list list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Grid = 

  let init x y = [ for j in [1..y] -> [ for i in [1..x] -> false ] ]

  let print grid = 
    seq {
      yield printfn "Display"
      for row in grid do
        for col in row do
          yield (if col then '#' else '.') |> printf "%c"
        yield printf "\n"
    } |> Seq.iter id

  let color x y = 
    List.mapi(fun rowI row -> 
        row |> List.mapi (fun colI col -> 
          match rowI < y, colI < x with
          | true, true -> true
          | _ -> col
    ))

  let rotateRow rX shift = 
    List.mapi(fun rowI row -> 
      let rowLength = row |> List.length
      row |> List.mapi(fun colI col -> 
        match rowI = rX with
        | true -> row.[(colI - shift + rowLength) % rowLength]
        | false -> col
    ))

  let rotateCol cY shift grid= 
    let colLength = grid |> List.length
    grid
    |> List.mapi(fun rowI row -> 
          let rowLength = row |> List.length
          row |> List.mapi(fun colI col -> 
            match colI = cY with
            | true -> grid.[(rowI - shift + colLength) % colLength].[colI]
            | false -> col
        ))

  let invoke grid i =
    match i with
    | Colour (x,y) -> color x y grid
    | RotateRow (x,y) -> rotateRow x y grid
    | RotateColumn (x,y) -> rotateCol x y grid

  let litCount grid = 
    seq { for row in grid do
            for col in row do
              yield col
    } |> Seq.filter(id) |> Seq.length

let partOne = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input") 
  |> Seq.map(Instruction.parse)
  |> Seq.fold Grid.invoke (Grid.init 50 6)
  |> Grid.litCount


let partTwo = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input") 
  |> Seq.map(Instruction.parse)
  |> Seq.fold Grid.invoke (Grid.init 50 6)
  |> Grid.print



