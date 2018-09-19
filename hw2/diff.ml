(*
 * 2015-11531, 이도형
 *)

type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list


exception InvalidArgument

let rec diff (ae, target) =
  match ae with
  | CONST n -> CONST 0
  | VAR x when x = target -> CONST 1
  | VAR _ -> CONST 0
  | POWER (x, n) when x = target -> TIMES [CONST n ; POWER (x, n - 1)]
  | POWER _ -> CONST 0
  | SUM [] -> raise InvalidArgument
  | SUM list -> SUM (List.map (fun elem -> diff (elem, target)) list)
  | TIMES [] -> raise InvalidArgument
  | TIMES list -> handle_times (list, target) and
handle_times (list, target) =
  let
    f = fun acc ae ->
          let reversed = List.rev list in
          let rest = List.filter ((!=) ae) reversed in
          (TIMES (diff (ae, target) :: rest)) :: acc
  in
    SUM (List.fold_left f [] list)



