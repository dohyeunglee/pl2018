(*
 * 2015-11531, 이도형
 *)

let rec prod f j i =
  match j with
  | 1 -> f (i, 1)
  | n -> (f (i, n)) *. prod f (n - 1) i


let rec sigma f n =
  match n with
  | 1 -> f 1
  | n -> (f n) +. sigma f (n - 1)

let sumprod (f, n, k) =
  let temp_f = prod f k in
  sigma temp_f n
