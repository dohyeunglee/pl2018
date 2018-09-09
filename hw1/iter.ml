(*
 * 2015-11531, 이도형
 *)

let id x = x
let compose f g x = f (g x)
let rec fold_left ~init ~f ~list = 
	match list with
    | [] -> init
	| hd :: rest -> fold_left ~init:(f init hd) ~f ~list:rest
let iter n f = 
	if n = 0 then id
    else if n < 0 then raise (Failure "n should be 0 or positive integer")
	else 
		let rec repeat f times = 
			match times with
			| 0 -> []
			| n -> f :: (repeat f (n-1))
		in	
			List.fold_left compose id (repeat f n)

let _ = 
	let test n = iter n (fun x -> x + 2) in
	Printf.printf "%d\n" (test 5 0)
