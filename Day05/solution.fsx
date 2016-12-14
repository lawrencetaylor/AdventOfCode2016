#load "./../common.fsx"

open Common

open System
open System.Text

let hasFiveLeadingZeros (str : string) = 
    let testString = String('0', 5)
    str.Substring(0, 5) = testString

let getNext seed = 
  Seq.initInfinite(fun i -> sprintf "%s%i" seed i)
  |> Seq.filter(Cryptography.md5 >> hasFiveLeadingZeros)
  |> Seq.map(Cryptography.md5 >> String.charAt 5)
  |> Seq.take 8

let partOne = getNext "abbhdwsy" |> String.ofChars // abbhdwsy

let getPassword (md5s : seq<string>) = 
  let pwd = Array.create 8 ' '
  let isPasswordFull a = a |> Seq.forall((<>) ' ')

  seq { for md5 in md5s do
          match md5 |> String.charAt 5 |> toString |> Int.tryParse with
          | Some i when i < 8 -> 
            match pwd.[i] with
            | ' ' -> 
              pwd.[i] <- (md5 |> String.charAt 6)
              match isPasswordFull pwd with
              | true -> yield pwd |> String.ofChars
              | false -> ()
            | _ -> ()
          | _ -> ()
  }

let getAdvancedPassword seed = 
  Seq.initInfinite(fun i -> sprintf "%s%i" seed i)
  |> Seq.filter(md5 >> hasFiveLeadingZeros)
  |> Seq.map(md5)
  |> getPassword
  |> Seq.head

let test = "abbhdwsy" |> getAdvancedPassword // 424a019