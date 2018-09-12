(*
 * 2015-11531, 이도형
 *)

type heap =
  | EMPTY
  | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function
  | EMPTY -> -1
  | NODE (r, _, _, _) -> r

let shake (x, lh, rh) =
  if (rank lh) >= (rank rh) then NODE (rank rh + 1, x, lh, rh)
  else NODE (rank lh + 1, x, rh, lh)

let rec merge =
  function
  | (EMPTY, EMPTY) -> EMPTY
  | ((NODE _ as n, EMPTY) | (EMPTY, (NODE _ as n))) -> n
  | ((NODE (_, x1, _, _) as heap1), (NODE (_, x2, _, _) as heap2)) ->
      let
        (NODE (_, x, lh, rh), target) = if x1 > x2 then (heap2, heap1) else (heap1, heap2)
      in
        shake (x, lh, merge (rh, target))

let insert (x, h) = merge (h, NODE(0, x, EMPTY, EMPTY))

let findMin = function
  | EMPTY -> raise EmptyHeap
  | NODE(_, x, _, _) -> x

let deleteMin = function
  | EMPTY -> raise EmptyHeap
  | NODE(_, x, lh, rh) -> merge (lh, rh)

let a = NODE (0, 8, NODE (0, 11, EMPTY, EMPTY), EMPTY);;
let b = NODE (0, 12, NODE (0, 15, EMPTY, EMPTY), EMPTY);;
let c = NODE (0, 7, NODE (0, 14, EMPTY, EMPTY), EMPTY);;
let first = NODE (1, 5, NODE (0, 6, EMPTY, EMPTY), b);;
let second = NODE (1, 3, c, a);;


