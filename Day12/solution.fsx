#load "./../common.fsx"

open Common

type Register = A | B | C | D
and Holder = Register of Register | Value of int
and Copy = Holder*Register
and JumpIfNonZero = Holder*int
and Inc = Register
and Dec = Register
and RegisterState = Map<Register,int>
and ProgramState = RegisterState*int
and StateChangeFn = ProgramState -> ProgramState

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Register = 
  let extractValue = Map.find

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Holder = 
  let extractValue = function | Value v -> (fun _ -> v) | Register r -> Register.extractValue r

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Instruction = 
  let copy (s, t) : StateChangeFn  = 
    let stateChange (rs : RegisterState, ptr) = 
      let valueToCopy = Holder.extractValue s rs
      (rs |> Map.add t valueToCopy, ptr + 1)
    stateChange

  let jump (h, i) : StateChangeFn = 
    let stateChange (rs : RegisterState, ptr) = 
      let holderValue = Holder.extractValue h rs
      if holderValue <> 0 then (rs, ptr + i)
      else (rs, ptr + 1)
    stateChange

  let inc r : StateChangeFn = 
    let stateChange (rs : RegisterState, ptr) = 
      let registerValue = Register.extractValue r rs
      (rs |> Map.add r (registerValue + 1), ptr + 1)
    stateChange

  let dec r : StateChangeFn = 
    let stateChange (rs : RegisterState, ptr) = 
      let registerValue = Register.extractValue r rs
      (rs |> Map.add r (registerValue - 1), ptr + 1)
    stateChange

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Parsing = 

  let parseRegister (str : string) = 
    match str.Trim() with
    | "a" -> A
    | "b" -> B
    | "c" -> C
    | "d" -> D
    |> Register

  let parseHolder (str : string) = 
    match str |> Int.tryParse with
    | None -> parseRegister str
    | Some i -> Value i

  let (|Copy|_|) (str : string) = 
    match str.StartsWith("cpy") with
    | true -> 
        let [ holder; Register value ] = str |> String.split ' ' |> List.ofSeq |> List.skip 1 |> List.map(parseHolder)
        Some (holder, value)
    | false  -> None

  let (|Inc|_|) (str : string) = 
    match str.StartsWith("inc") with
    | true -> 
        let [ Register value ] = str |> String.split ' ' |> List.ofSeq |> List.skip 1 |> List.map(parseHolder)
        Some (value)
    | false  -> None

  let (|Dec|_|) (str : string) = 
    match str.StartsWith("dec") with
    | true -> 
        let [ Register value ] = str |> String.split ' ' |> List.ofSeq |> List.skip 1 |> List.map(parseHolder)
        Some (value)
    | false  -> None

  let (|Jump|_|) (str : string) = 
    match str.StartsWith("jnz") with
    | true ->  
        let [ holder ; Value value ; ] = str |> String.split ' ' |> List.ofSeq |> List.skip 1 |> List.map(parseHolder)
        Some (holder, value)
    | false  -> None

  let parse = function
    | Copy x -> Instruction.copy x
    | Inc x -> Instruction.inc x
    | Dec x -> Instruction.dec x
    | Jump x -> Instruction.jump x
    | y -> failwithf "Unparsable instruction: %s" y

let program initialState = 
  Seq.unfold(fun (registerState, ptr) ->
    match test.TryFind ptr with
    | None -> None
    | Some stateChange -> 
        let newState = stateChange (registerState, ptr)
        Some (newState, newState)
    ) initialState
  |> Seq.last

let partOne = ([A, 0; B, 0; C, 0; D, 0] |> Map.ofList, 0) |> program
let partTwo = ([A, 0; B, 0; C, 1; D, 0] |> Map.ofList, 0) |> program



