#load "./../common.fsx"
#load "./shared.fsx"

open Common
open Shared

let input = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "./input")
  |> Array.mapi(fun i s -> (i, Parsing.parse s))
  |> Map.ofSeq

let program (initialState : ProgramState) = 
  Seq.unfold(fun (registerState, ptr) ->
    match input.TryFind ptr with
    | None -> None
    | Some instruction -> 
        let newState = Instruction.execute instruction (registerState, ptr)
        Some (newState, newState)
    ) initialState
  |> Seq.last

let partOne = ([A, 0; B, 0; C, 0; D, 0] |> Map.ofList, 0) |> program // 318009
let partTwo = ([A, 0; B, 0; C, 1; D, 0] |> Map.ofList, 0) |> program // 9227663



