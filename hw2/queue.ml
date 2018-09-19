(*
 * 2015-11531, 이도형
 *)

module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = (int list) list * (int list) list
    exception EMPTY_Q

    let emptyQ = ([], [])
    let enQ ((inbox, outbox), element) = (element :: inbox, outbox)
    let deQ =
      function
      | ([], []) -> raise EMPTY_Q
      | (inbox, hd :: rest) -> (hd, (inbox, rest))
      | (inbox, []) ->
          let
            pop :: rest = List.rev inbox
          in (pop, ([], rest))
  end

