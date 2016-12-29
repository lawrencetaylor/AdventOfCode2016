#load "./../common.fsx"
#load "./../Day12/shared.fsx"

open Common
open Shared

type InstructionEx = 
  | Instruction of Instruction
  | Toggle of Register
  | Skip
  | AddMultipleToA

type InstructionState = Map<int, InstructionEx>
type ProgramStateEx = InstructionState*ProgramState

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InstructionEx = 

  let execute (instruction : InstructionEx) ((instructionState : InstructionState, (registerState, ptr) : ProgramState) : ProgramStateEx)= 
    match instruction  with
    | Toggle r -> 
      let registerValue = registerState |> Register.extractValue r
      let newInstructionState = 
        instructionState 
        |> Map.map(fun p ins -> 
          match p = (registerValue + ptr) with
          | false -> ins
          | true -> 
            match ins with
            | Instruction (Inc a) -> Instruction (Dec a)
            | Instruction (Dec a) -> Instruction (Inc a)
            | Toggle a -> Instruction (Inc a)
            | Instruction (JumpIfNonZero (h, i)) -> 
              match i with
              | Register reg -> Instruction (Copy (h, reg))
              | _ -> Skip
            | Instruction (Copy (h, r)) -> Instruction (JumpIfNonZero (h, Register r))
            | Skip -> Skip
          )
      (newInstructionState, (registerState, ptr + 1))
    | Skip -> (instructionState, (registerState, ptr + 1))
    | AddMultipleToA -> 
      let bVal = registerState.[B]
      let dVal = registerState.[D]
      let aVal = registerState.[A]
      let newReg = 
        registerState
          .Add(A, aVal + bVal * dVal)
          .Add(B, bVal)
          .Add(C, 0)
          .Add(D, 0)
      (instructionState, (newReg, ptr + 1))
    | Instruction ins -> (instructionState , Instruction.execute ins (registerState, ptr))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParsingEx = 

  (*
    Extends the parsing of Day 12 to account for the Toggle Instruction
  *)

  let (|Toggle|_|) (str : string) = 
    match str.StartsWith("tgl") with
    | true -> 
        let [ Register register ] = str |> String.split ' ' |> List.ofSeq |> List.skip 1 |> List.map(Parsing.parseHolder)
        Some (register)
    | false  -> None

  let parse = function
    | Toggle x -> Toggle x
    | s -> Instruction (Parsing.parse s)
  
let program (initialState : ProgramStateEx) = 
  Seq.unfold(fun ((instructionState : InstructionState, (registerState, ptr)), counter) ->
    match instructionState.TryFind ptr with
    | None -> None
    | Some instruction -> 
        let newState = InstructionEx.execute instruction (instructionState, (registerState, ptr))
        let (_, (registers, _)) = newState
        Some (registers.[A], (newState, counter + 1))
    ) (initialState, 0)
  |> Seq.last

let rawInstructions = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "./input")
  |> Array.map(ParsingEx.parse)
  |> List.ofArray

let testInput = 
  rawInstructions
  |> List.mapi(fun i ins -> (i, ins))
  |> Map.ofList

(*
    cpy b c     |
    inc a       |
    dec c       |
    jnz c -2    | Adds a copy of what is in B to A
    dec d
    jnz d -5    |  Does this d times
*)
let optimise instructions = 
  let rec optimisationStepInner instructions = 
    match instructions with 
    | ins when (List.take 6 ins) = 
      ( [ Copy (Register B, C)
          Inc A
          Dec C
          JumpIfNonZero (Register C, Value -2)
          Dec D
          JumpIfNonZero (Register D, Value -5)
        ] |> List.map Instruction ) -> 
          (AddMultipleToA::Skip::Skip::Skip::Skip::Skip::(ins |> List.skip 6))
    | x::xs -> x::(optimisationStepInner xs)
  optimisationStepInner instructions

let optimisedInput = 
  rawInstructions 
  |> optimise
  |> List.mapi(fun i ins -> (i, ins))
  |> Map.ofList

let partOne = (optimisedInput, ([A, 7; B, 0; C, 0; D, 0] |> Map.ofList, 0)) |> program // 10886
let partTwo = (optimisedInput, ([A, 12; B, 0; C, 0; D, 0] |> Map.ofList, 0)) |> program // 479007446






