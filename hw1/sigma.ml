(*
 * 2015-11531, 이도형
 *)

let rec _sigma a b f sum =
  if a = b then sum + f b
  else if a > b then 0
  else _sigma (a + 1) b f (sum + f a)

let sigma (a, b, f) = _sigma (a+1) b f (f a)
