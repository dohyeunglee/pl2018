(*
 * 2015-11531, 이도형
 *)

let rec _sigma a b f sum = 
	if a = b then sum + f b
	else if a > b then raise(Failure "b should be greater than a")
	else _sigma (a + 1) b f (sum + f a)

let sigma a b f = _sigma (a+1) b f (f a)

let _ =
	let result = sigma 3 5 (fun x -> x + 1)
	in Printf.printf "%d\n" result
