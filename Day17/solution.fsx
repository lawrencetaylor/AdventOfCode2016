#load "./../common.fsx"
open Common
open System

type Move = U | D | L | R
type Position = int*int

let move (x,y) = function
  | U -> (x, y - 1)
  | D -> (x, y + 1)
  | L -> (x - 1, y)
  | R -> (x + 1, y)

let isValid (x,y) = 
  x >= 1 &&
  x <= 4 &&
  y >= 1 &&
  y <= 4

let moveToString = function
  | U -> "U"
  | D -> "D"
  | L -> "L"
  | R -> "R"

let isOpen c = ['b';'c';'d';'e';'f'] |> List.contains c

let possibleDirections init = 
  [U; D; L; R]
  |> List.filter(fun m -> (move init m) |> isValid)

let getOpen allPossible (hash : string) = 
  hash.Substring(0, 4)
  |> Seq.mapi(fun i c -> (i, isOpen c))
  |> Seq.filter(snd)
  |> Seq.map(fst >> function | 0 -> U | 1 -> D | 2 -> L | 3 -> R)
  |> Seq.filter(fun c -> allPossible |> Seq.contains c)

let nextMoves (thisHash, thisPosition) = 
  let possible = possibleDirections thisPosition
  getOpen possible (Cryptography.md5 thisHash)
  |> Seq.map(fun m -> (sprintf "%s%s" thisHash (m |> moveToString), move thisPosition m))

let bfs (seed : string) (position : Position) = 
  let q = new System.Collections.Generic.Queue<_>()
  q.Enqueue((seed, position))
  seq {
    while q.Count > 0 do
      let (lastHash, pos) = q.Dequeue()
      if pos = (4,4) then 
        yield lastHash
      else 
        nextMoves (lastHash, pos)
        |> Seq.iter(q.Enqueue) 
  }

(*
  Test cases: 
  If your passcode were ihgpwlah, the shortest path would be DDRRRD.
  With kglvqrro, the shortest path would be DDUDRLRRUDRD.
  With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR
*)

let run seed = 
  let lastHash = bfs seed (1,1) |> Seq.head
  lastHash.Substring(seed.Length)

(*
let tests = 
  [ "ihgpwlah"
    "kglvqrro"
    "ulqzkmiv"
  ] |> List.map(run) 
*)

let partOne = run "veumntbg" // DDRRULRDRD

let longest seed = 
  bfs seed (1,1) 
  |> Seq.last
  |> fun d -> d.Length - seed.Length

(*
let tests2 = 
  [ "ihgpwlah"
    "kglvqrro"
    "ulqzkmiv"
  ] |> List.map(longest)
*)

let partTwo = longest "veumntbg" // 536