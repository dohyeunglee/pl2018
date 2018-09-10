(* 
 * 2015-11531, 이도형
 *)

type nat = ZERO | SUCC of nat

let rec natadd (x, y) =
  match x with
  | ZERO -> y
  | SUCC n -> SUCC (natadd (n, y))

let rec natmul (x, y) = 
  match x with
  | ZERO -> ZERO
  | SUCC n -> natadd (x, (natmul (n, y)))
