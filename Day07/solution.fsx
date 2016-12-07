#load "./../common.fsx"

open Common

open System.Text.RegularExpressions

type IP7 = { Supernet : string list; Hypernet : string list }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IP7 =

  let parse str = 
    let toReverseString = List.rev >> String.ofChars
    let rec innerParse (charList : char list) (currentWord : char list) (nonHyperNet : string list) (hyperNet : string list) = 
      match charList with 
      | '['::xs -> innerParse xs [] (toReverseString currentWord::nonHyperNet) hyperNet
      | ']'::xs -> innerParse xs [] nonHyperNet (toReverseString currentWord::hyperNet)
      | x  ::xs -> innerParse xs (x::currentWord) nonHyperNet hyperNet
      | [] -> (toReverseString currentWord::nonHyperNet, hyperNet)

    let (nonHyper, hyper) = innerParse (str |> List.ofSeq) [] [] []
    { Supernet = nonHyper; Hypernet = hyper}

  let private isABBA (str : string) = 
    let rec isABBAInner charList =
      match charList with 
      | a::b::c::d::xs when a = d && b = c && a <> b -> true
      | x::xs -> 
        if xs |> List.length > 3 then isABBAInner xs
        else false
    isABBAInner (str |> List.ofSeq)

  let supportsTLS ip = 
    ip.Hypernet |> List.forall(isABBA >> not)
    && ip.Supernet |> List.exists(isABBA)

  let private getABASequences (str : string) = 
    let rec getABASequencesInner charList currentSequence= 
      match charList with
      | a::b::c::xs when a = c && a <> b -> getABASequencesInner (b::c::xs) (([a;b;c] |> String.ofChars)::currentSequence)
      | x::xs -> 
        if xs |> List.length > 2 then getABASequencesInner xs currentSequence
        else currentSequence
    getABASequencesInner (str |> List.ofSeq) []

  let private mirror (block : string) = 
    let [a;b;c] = block |> List.ofSeq
    [b;a;b] |> String.ofChars

  let supportsSSL ip = 
    ip.Supernet 
    |> List.collect(getABASequences) 
    |> List.distinct
    |> List.exists(fun aba -> ip.Hypernet |> List.exists(aba |> mirror |> String.contains))

let ipAdresses = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input") 
  |> Seq.map(IP7.parse) 

let partOne = 
  ipAdresses
  |> Seq.filter(IP7.supportsTLS)
  |> Seq.length // 110
let partTwo = 
  ipAdresses
  |> Seq.filter(IP7.supportsSSL)
  |> Seq.length // 242
