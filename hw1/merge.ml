(*
 * 2015-11531, 이도형
 *)

let merge a b = 
	let concat = a @ b in
	List.sort (fun x y -> - compare x y) concat

