#load "./../common.fsx"

open Common

let (|Triple|_|) (str : string) = 
  let rec innerTriple c = 
    match c with
    | l when l |> List.length < 3 -> None
    | a::b::c::_ when a = b && b = c -> Some a
    | x::xs -> innerTriple xs

  innerTriple (str |> List.ofSeq)

let has5Of c (str : string) = 
  str.Contains(System.String(c,5))

let rec matches hashingAlgorithm i triples = 
  seq {
    let hash = i |> hashingAlgorithm
    let (successfulKeys, notMatched) = 
      triples
      |> List.fold(fun (keys, pendingKeys ) (j, c,a)  -> 
        match hash |> has5Of c with 
        | true -> 
          ((j, a)::keys, pendingKeys)
        | false -> (keys, (j, c, a + 1)::pendingKeys)
        ) ([], [])
    for (k, idx) in successfulKeys |> List.rev do 
      (k, idx) |> log "Found"
      yield k

    let pendingKeysToTakeForward = notMatched |> List.filter(fun (_,_,k) -> k < 1000)
    let toGoForward = 
      match hash with 
      | Triple c -> (i, c, 0)::pendingKeysToTakeForward
      | _ -> pendingKeysToTakeForward

    yield! matches hashingAlgorithm (i+1) toGoForward
  }

let getHash i = 
  sprintf "zpqevtbw%i" i |> Cryptography.md5

(*
  Have to go a bit "above" the target to make sure we get 1000-64th-attempts steps ahead of what we think is the 64th one
  in case there is a lower number which gets 5 hashes in a larger number of attempts.
*)
let partOne = matches getHash 0 [] |> Seq.take 100 |> Seq.sort |> Seq.take 64 |> Seq.last

let stretchedHash i = 
  [1..2016]
  |> List.fold(fun lastHash _ -> Cryptography.md5 lastHash) (getHash i)

// This takes a while
let partTwo = matches stretchedHash 0 [] |> Seq.take 70 |> Seq.sort |> Seq.take 64 |> Seq.last


  


