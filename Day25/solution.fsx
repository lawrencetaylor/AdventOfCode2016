#load "./../common.fsx"
#load "./../Day12/shared.fsx"

open Shared
open Common

type InstructionEx = 
  | Instruction of Instruction
  | Out of Register

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InstructionEx = 

  (*
    The execution of an `Out` command does not change the state, so we ignore it
    here and deal with it when we pipe stuff to the output
  *)

  let execute (instruction : InstructionEx) ((registerState, ptr) : ProgramState)= 
    match instruction  with
    | Out r -> (registerState, ptr + 1)
    | Instruction ins -> Instruction.execute ins (registerState, ptr)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParsingEx = 

  (*
    Extends the parsing logic for Day 12 to handle the `Out` command
  *)

  let (|Out|_|) (str : string) = 
    match str.StartsWith("out") with
    | true -> 
        let [ Register register ] = str |> String.split ' ' |> List.ofSeq |> List.skip 1 |> List.map(Parsing.parseHolder)
        Some (register)
    | false  -> None

  let parse = function
    | Out x -> Out x
    | s -> Instruction (Parsing.parse s)


(*
  Creates a `seq<int>` representing the signal
*)
let rec nextOutput (instructions : Map<_,_>) (registerState : RegisterState) ( ptr : int) = 
  seq {
    match instructions.TryFind ptr with
    | None -> ()
    | Some i -> 
      match i with
      | Out x -> 
        yield (Register.extractValue x registerState)
        yield! nextOutput instructions registerState (ptr + 1)
      | i -> 
        let (newState, newPtr) = InstructionEx.execute i (registerState, ptr)
        yield! nextOutput instructions newState newPtr
  }

let rawInstructions = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "./input")
  |> Array.map(ParsingEx.parse)
  |> List.ofArray
  |> List.mapi(fun i ins -> (i, ins))
  |> Map.ofList

let getSequence seed = 
  nextOutput rawInstructions ([A, seed; B, 0; C, 0; D, 0] |> Map.ofList) 0

(*
  This is cheating really, since I do not offer any proof that the sequence which 
  satisfies this predicate really will repeat indefinately.

  Apparently the signal has a cycle length of 12 so this would suffice.
*)
let isAlternating s = 
  s |> Seq.take 14 |> Seq.toList |> log "reault" |> (=) [0;1;0;1;0;1;0;1;0;1;0;1;0;1]

let s = 
  Seq.initInfinite id
  |> Seq.map(fun seed -> (seed, getSequence seed))
  |> Seq.filter (snd >> isAlternating)
  |> Seq.map fst
  |> Seq.head

    