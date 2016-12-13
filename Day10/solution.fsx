#load "./../common.fsx"

open Common

type Chip = Chip of int


type ChipTarget = Bot of int | Output of int
type BotBehaviour = ChipTarget*ChipTarget

let getOutputs (bb : BotBehaviour) = 
  match bb with
  | Output i, Output j -> [i; j]
  | Output i, _ -> [i]
  | _, Output j -> [j]
  | _ -> []

let (|BotDefinition|_|) str = 
  match 
    Regex.tryMatch "bot (\d+) gives low to bot (\d+) and high to bot (\d+)" str, 
    Regex.tryMatch "bot (\d+) gives low to output (\d+) and high to bot (\d+)" str,
    Regex.tryMatch "bot (\d+) gives low to bot (\d+) and high to output (\d+)" str,
    Regex.tryMatch "bot (\d+) gives low to output (\d+) and high to output (\d+)" str with
  | Some matches, _, _, _-> 
      let [botId ; lowBot ; highBot] = matches |> List.map(Int.parse)
      (botId, (Bot lowBot, Bot highBot)) |> Some
  | None, Some matches, _, _ -> 
      let [botId ; lowOut ; highBot] = matches |> List.map(Int.parse)
      (botId, (Output lowOut, Bot highBot)) |> Some
  | None, None, Some matches, _ -> 
      let [botId ; lowBot ; highOut] = matches |> List.map(Int.parse)
      (botId, (Bot lowBot, Output highOut)) |> Some
  | None, None, None, Some matches -> 
      let [botId ; lowOut ; highOut] = matches |> List.map(Int.parse)
      (botId, (Output lowOut, Output highOut)) |> Some
  | _ -> None

type BotId = int
type InitialState = BotId*Chip

let (|InitialStateDefinition|_|) = 
  Regex.tryMatch "value (\d+) goes to bot (\d+)"
  >> Option.map(fun matches -> 
      let [chipValue; botId] = matches |> List.map(Int.parse)
      (botId, Chip chipValue))

type T = ((BotId*BotBehaviour) list)*(InitialState list)
let defaultT : T = ([],[])

let parseFold (bots : Map<BotId,BotBehaviour>, inits) str = 
  match str with
  | BotDefinition (b,c) -> (bots |> Map.add b c,  inits)
  | InitialStateDefinition i -> (bots, i::inits)

let (botBehaviours, initials) = 
//  [
//    "value 5 goes to bot 2"
//    "bot 2 gives low to bot 1 and high to bot 0"
//    "value 3 goes to bot 1"
//    "bot 1 gives low to output 1 and high to bot 0"
//    "bot 0 gives low to output 2 and high to output 0"
//    "value 2 goes to bot 2"
//  ] 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input")
  |> Array.toList
  |> List.fold(parseFold) (Map.empty,[])

let allBots = botBehaviours |> Map.toList |> List.map(fst)
let allOutputs = botBehaviours |> Map.toList |> List.collect(snd >> getOutputs) |> List.map(Output)

let initMap = 
  initials
  |> List.groupBy(fst)
  |> List.map(fun (botId, is) -> (botId, is |> List.map(snd)))
  |> Map.ofList

type BotsState = Map<ChipTarget,Chip list>

// All state contains
// Bots with chips
// Output with chips
let allState : BotsState = 
  allBots
  |> List.map(fun b -> 
    match initMap.TryFind b with
    | None -> (Bot b, [])
    | Some d -> (Bot b, d))
  |> List.append (allOutputs |> List.map(fun o -> (o, [])))
  |> Map.ofList

let isOutput = function | Bot _ -> false | _ -> true
let isBot = isOutput >> not

(*
  Runs a specific Bot given the current state of all the bots
  Return (switchMade option, newState)
*)
let run bot (thisState : BotsState) = 
  let (lowTarget, highTarget) = botBehaviours.[bot]

  let processLow  bot chip (state : BotsState) = 
    if isOutput lowTarget || thisState.[lowTarget].Length < 2 then
      state 
      |> Map.add lowTarget (chip::state.[lowTarget])
    else state

  let processHigh  bot chip (state : BotsState)= 
    if isOutput highTarget || thisState.[highTarget].Length < 2 then
      state 
      |> Map.add highTarget (chip::state.[highTarget])
    else state
  match thisState.[(Bot bot)] |> List.sort with
  | [] -> (bot, None, thisState)
  | [c] -> (bot, None, thisState)
  | [c';c''] -> 
    let [low;high] = [c';c''] |> List.sort
    (bot, Some (low, high),
      thisState 
      |> processLow bot low
      |> processHigh bot high
      |> Map.remove (Bot bot)
      |> Map.add (Bot bot) [] )

// Given the current bots state, gets a next possible move
let nextMove (botState : Map<ChipTarget, Chip list>) = 
  match 
    botState 
    |> Map.toSeq
    |> Seq.filter(fun (b, l) -> isBot b && l.Length = 2)
    |> Seq.tryHead with
  | None -> None
  | Some (Bot bot, _) -> Some (run bot botState)

// Runs the bots from the given initial state
let programSequence = 
  allState 
  |> Seq.unfold(fun s -> 
    match nextMove s with
    | None -> None
    | Some (bot, switch, s) -> 
      Some ((bot, switch, s),s))

let partOne = 
  programSequence
  |> Seq.filter((fun (_,a,_) -> a) >> (=) (Some (Chip 17, Chip 61)))
  |> Seq.head

let partTwo = 
  programSequence
  |> Seq.last
  |> fun (a,b,c) -> c
  |> fun s -> 
    let ([Chip c0], [Chip c1], [Chip c2]) = (s.[Output 0],s.[Output 1],s.[Output 2])
    c0*c1*c2




