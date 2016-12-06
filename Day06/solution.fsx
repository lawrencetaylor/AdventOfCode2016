#load "./../common.fsx"

open Common

let input = [
"eedadn"
"drvtee"
"eandsr"
"raavrd"
"atevrs"
"tsrnev"
"sdttsa"
"rasrtv"
"nssdts"
"ntnada"
"svetve"
"tesnvt"
"vntsnd"
"vrdear"
"dvrsen"
"enarar"
]

let parse sortF (strings : seq<string>) = 
  seq {
    for str in strings do
      for i in 0..str.Length - 1 do
        yield (i, str.[i])
  }
  |> Seq.groupBy(fst)
  |> Seq.map(
      snd 
      >> Seq.map(snd) 
      >> Seq.groupBy id 
      >> Seq.map(fun (c,i) -> (c, i |> Seq.length))
      >> Seq.sortBy(fun (_, f) -> f |> sortF)
      >> Seq.head
      >> fst)
  |> String.ofChars

let partOne = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input") |> parse id
let partTwo = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input") |> parse (fun x -> -x)
