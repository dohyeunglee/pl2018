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
      let diff = int_a - int_b in
      match diff with
      | 0 -> f (float_of_int int_a)
      | _ -> f (float_of_int int_a) +. sigma (float_of_int (int_a + 1)) b f

let rec quad_by_parts a b f =
  if a < b then (0.1 *. f a) +. quad_by_parts (a +. 0.1) b f
  else 0.

let rec integral a b f =
  let diff = a -. b in
  if abs_float diff < 0.1 then 0.
  else if diff > 0. then (-. integral b a f)
  else quad_by_parts a b f

let rec expr_to_fun exp =
  match exp with
  | X -> id
  | INT n -> (fun _ -> float_of_int n)
  | REAL n -> (fun _ -> n)
  | ADD (left, right) -> (fun x -> (expr_to_fun left) x +. (expr_to_fun right) x)
  | SUB (left, right) -> (fun x -> (expr_to_fun left) x -. (expr_to_fun right) x)
  | MUL (left, right) -> (fun x -> (expr_to_fun left) x *. (expr_to_fun right) x)
  | DIV (left, right) -> (fun x -> (expr_to_fun left) x /. (expr_to_fun right) x)
  | SIGMA _ -> (fun _ -> mathemadiga exp)
  | INTEGRAL _ -> (fun _ -> mathemadiga exp) and
  mathemadiga =
    function
    | X -> raise FreeVariable
    | INT n -> float_of_int n
    | REAL n -> n
    | ADD (left, right) -> mathemadiga left +. mathemadiga right
    | SUB (left, right) -> mathemadiga left -. mathemadiga right
    | MUL (left, right) -> mathemadiga left *. mathemadiga right
    | DIV (left, right) -> mathemadiga left /. mathemadiga right
    | SIGMA (a, b, exp)
        -> sigma (mathemadiga a) (mathemadiga b) (expr_to_fun exp)
    | INTEGRAL (a, b, exp)
        -> integral (mathemadiga a) (mathemadiga b) (expr_to_fun exp)


