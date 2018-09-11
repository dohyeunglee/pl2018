(*
 * 2015-11531, 이도형
 *)

type ae
  | CONST of int
  | VAR of string
  | TIMES of ae list
  | SUM of ae list

let rec diff (ae, target) =
  match ae with
  | CONST n -> 0
  | VAR variable ->

let _ =
  let
    ax_2 = MUL [VAR "a" ; POWER ("x", 2)] and
    bx = MUL [VAR "b" ; VAR "x"] and
    c = VAR "c" and
    two_ax = MUL [CONST 2 ; VAR "a" ; VAR "x"] and
    b = VAR "b"
    expected = SUM [two_ax ; b]
  in
    let test = SUM [ax_2 ; bx ; c] in
    expected = diff test



