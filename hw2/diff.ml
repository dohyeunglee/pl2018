(*
 * 2015-11531, 이도형
 *)

type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list


let minimize =
  function
  | SUM ->
  | TIMES ->
  | x -> x


let rec diff (ae, target) =
  match ae with
  | CONST n -> CONST 0
  | VAR x when x = target -> CONST 1
  | VAR _ -> CONST 0
  | POWER (x, n) when x = target -> TIMES [CONST n ; POWER (x, n - 1)]
  | POWER _ -> CONST 0
  | SUM list -> SUM (List.map (fun elem -> diff (elem, target)) list)
  | TIMES list -> handle_times (list, target) and
handle_times (list, target) =
  let
    f = fun acc ae ->
          let reversed = List.rev list in
          let rest = List.filter ((!=) ae) reversed in
          (TIMES (diff (ae, target) :: rest)) :: acc
  in
    SUM (List.fold_left f [] list)




let
  ax_2 = TIMES [VAR "a" ; POWER ("x", 2)] and
  bx = TIMES [VAR "b" ; VAR "x"] and
  c = VAR "c" and
  two_ax = TIMES [CONST 2 ; VAR "a" ; VAR "x"] and
  b = VAR "b"
let
  expected = SUM [two_ax ; b] and
  test = SUM [ax_2 ; bx ; c]



