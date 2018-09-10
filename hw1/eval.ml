type formula =
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec eval_expr =
  function
  | NUM n -> n
  | PLUS (left, right) -> eval_expr left + eval_expr right
  | MINUS (left, right) -> eval_expr left - eval_expr right

let rec eval =
  function
  | TRUE -> true
  | FALSE -> false
  | NOT formula -> not (eval formula)
  | ANDALSO (left, right) -> (eval left) && (eval right)
  | ORELSE (left, right) -> (eval left) || (eval right)
  | IMPLY (left, right) ->
      let left_val = eval left and right_val = eval right in
      if left_val = true && right_val = false then false
      else true
  | LESS (left, right) -> eval_expr left < eval_expr right
