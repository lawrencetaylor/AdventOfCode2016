#load "./../common.fsx"

open Common

type Room = { SectorId : int; CheckSum : string; Name : string}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Room = 
  let parse (str : string) = 
    let [name_sectorId; checkSumWithBraket ] = str |> String.split '[' |> List.ofSeq
    let checkSum = checkSumWithBraket.TrimEnd(']')
    let sectorId::namerev = name_sectorId |> String.split '-' |> Seq.rev |> List.ofSeq
    {
      SectorId = sectorId |> Int.parse
      CheckSum = checkSum
      Name = namerev |> List.rev |> String.concat " "
    }

  let sectorId room = room.SectorId
  let name room = room.Name

  let verifyCheckSum (room : Room) = 
    let rec checkSumInner (s : char list) (m : Map<_,_>) = 
      match s with
      | [] -> m
      | x :: xs -> 
        match m |> Map.tryFind x with
        | None -> checkSumInner xs (m |> Map.add x 1)
        | Some v -> checkSumInner xs (m |> Map.add x (v+1))
    checkSumInner (room.Name |> Seq.filter((<>) ' ') |> List.ofSeq) Map.empty
    |> Map.toSeq
    |> Seq.sortBy(fun a -> (- snd a, fst a))
    |> Seq.map(fst)
    |> Array.ofSeq
    |> Array.take 5
    |> System.String
    |> (=) room.CheckSum

  let private rotateChar (times : int) (c : char) = 
    c |> System.Convert.ToInt32 |> minus 97  |> (+) times |> modulo 26 |> (+) 97 |> System.Convert.ToChar

  let private rotateWord times (word : string) = 
    word 
    |> Seq.map(rotateChar times)
    |> String.ofChars

  let decrypt (room : Room) = 
    { room with 
        Name = room.Name
                |> String.split('-')
                |> Seq.map(rotateWord room.SectorId)
                |> String.concat " " }

let rooms = 
  System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input") 
  |> Seq.map(Room.parse) 

let partOne = 
  rooms
  |> Seq.filter(Room.verifyCheckSum) 
  |> Seq.sumBy(fun r -> r.SectorId)

let partTwo = 
  rooms
  |> Seq.filter(Room.verifyCheckSum)
  |> Seq.map(Room.decrypt)
  |> Seq.filter(Room.name >> String.contains "north")
  |> Seq.map(fun r -> (r |> Room.name, r |> Room.sectorId))
  |> List.ofSeq
