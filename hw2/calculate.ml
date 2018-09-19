(*
 * 2015-11531, 이도형
 *)

type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

let id x = x
exception FreeVariable

let rec sigma a b f =
  let
    int_a = int_of_float a and
    int_b = int_of_float b
  in
    if int_a > int_b then 0.
    else
      let
        diff = int_a - int_b and
        float_a = float_of_int int_a
      in
        match diff with
        | 0 -> f float_a
        | _ -> f float_a +. sigma (float_a +. 1.) b f

let rec integral a b f =
  let diff = a -. b in
  if abs_float diff < 0.1 then 0.
  else if diff > 0. then (-. integral b a f)
  else
    (0.1 *. f a) +. integral (a +. 0.1) b f

let rec expr_to_fun exp =
  match exp with
  | X -> id
  | INT n -> (fun _ -> float_of_int n)
  | REAL n -> (fun _ -> n)
  | ADD (left, right) -> (fun x -> (expr_to_fun left) x +. (expr_to_fun right) x)
  | SUB (left, right) -> (fun x -> (expr_to_fun left) x -. (expr_to_fun right) x)
  | MUL (left, right) -> (fun x -> (expr_to_fun left) x *. (expr_to_fun right) x)
  | DIV (left, right) -> (fun x -> (expr_to_fun left) x /. (expr_to_fun right) x)
  | SIGMA _ -> (fun _ -> calculate exp)
  | INTEGRAL _ -> (fun _ -> calculate exp) and
  calculate =
    function
    | X -> raise FreeVariable
    | INT n -> float_of_int n
    | REAL n -> n
    | ADD (left, right) -> calculate left +. calculate right
    | SUB (left, right) -> calculate left -. calculate right
    | MUL (left, right) -> calculate left *. calculate right
    | DIV (left, right) -> calculate left /. calculate right
    | SIGMA (a, b, exp)
        -> sigma (calculate a) (calculate b) (expr_to_fun exp)
    | INTEGRAL (a, b, exp)
        -> integral (calculate a) (calculate b) (expr_to_fun exp)


