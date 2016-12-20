#load "./../common.fsx"
open Common

// Usual modulo operator gives values (0..n-1). We need (1..n)
let oneBasedModulo n = modulo n >> function | 0 -> n | i -> i

(*
  The trick here is to do it via recursion
  If I know which elf gets all the gold in a circle of size n-1, 
  when I take out the first elf I can work out who is safe.

  The only difference between part 1 and 2 is chosing which elf gets knocked out first.

  For part 1 it is always Elf 3 (modulo the size of the circle)
  For part 2 we need to calculate the half way point.

  The next go starts from Elf 2. If the safe elf (rebasing the circle at 2) is 
  before the knocked out elf, we are done.  Otherwise we need to add 1 to accont
  for the missing elf.
*)

let safe lastSafe n = 
  let stolenFromElf = 3
  if (1 + lastSafe) < stolenFromElf then (1 + lastSafe) |> oneBasedModulo n
  else (2 + lastSafe) |> oneBasedModulo n


let safe2 lastSafe n = 
  let stolenFromElf = ((int) (n + 2) / 2)
  if (1 + lastSafe) < stolenFromElf then (1 + lastSafe) |> oneBasedModulo n
  else (2 + lastSafe) |> oneBasedModulo n

let getSafe calc safeFor2 n  = 
  Seq.unfold(fun (lastSafe, thisN) -> 
  let thisSafe = calc lastSafe thisN
  Some ((thisN, thisSafe), (thisSafe, thisN + 1))) (safeFor2, 2)
  |> Seq.take  (n-1)
  |> Seq.last

let partOneAnswer = getSafe safe 1
let partOne = partOneAnswer 3001330 //1808358

let partTwoAnswer = getSafe safe2 1 
let partTwo = partTwoAnswer 3001330 //1407007

(*
  Part one (I later found out) is a straight forward application of Josephus' problem (https://en.wikipedia.org/wiki/Josephus_problem), and there
  exist closed formulae for both parts 1 and 2, but I found the algorithmic approach a bit easier to understand.
*)



  




