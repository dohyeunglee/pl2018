(*
 * SNU 4190.310 Programming Languages 2018 Fall
 *  K- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0) let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)

  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let apply_numeric_arithmetic v1 v2 f = 
    let applied = f (value_int v1) (value_int v2) in
    Num applied

  let apply_equal v1 v2 = 
    match (v1, v2) with
    | (Num n1, Num n2) -> Bool (n1 = n2)
    | (Bool b1, Bool b2) -> Bool (b1 = b2)
    | (Unit, Unit) -> Bool true
    | _ -> Bool false

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
       | Addr l -> l
       | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
       | Addr _ -> raise (Error "TypeError : not proc") 
       | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | NUM n -> (Num n, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR x -> 
      let address = lookup_env_loc env x in
      let value = Mem.load mem address in
      (value, mem)
    | ADD (e1, e2) -> 
      let (val1, mem') = eval mem env e1 in
      let (val2, mem'') = eval mem' env e2 in
      (apply_numeric_arithmetic val1 val2 (+), mem'')
    | SUB (e1, e2) -> 
      let (val1, mem') = eval mem env e1 in
      let (val2, mem'') = eval mem' env e2 in
      (apply_numeric_arithmetic val1 val2 (-), mem'')
    | MUL (e1, e2) -> 
      let (val1, mem') = eval mem env e1 in
      let (val2, mem'') = eval mem' env e2 in
      (apply_numeric_arithmetic val1 val2 ( * ), mem'')
    | DIV (e1, e2) -> 
      let (val1, mem') = eval mem env e1 in
      let (val2, mem'') = eval mem' env e2 in
      (apply_numeric_arithmetic val1 val2 (/), mem'')
    | EQUAL (e1, e2) ->
      let (val1, mem') = eval mem env e1 in
      let (val2, mem'') = eval mem' env e2 in
      let result = apply_equal val1 val2 in
      (result, mem'')
    | LESS (e1, e2) ->
      let (val1, mem') = eval mem env e1 in
      let (val2, mem'') = eval mem' env e2 in
      let result = Bool (value_int val1 < value_int val2) in
      (result, mem'')
    | NOT e ->
      let (v, mem') = eval mem env e in
      let result = Bool (not (value_bool v)) in
      (result, mem')
    | SEQ (e1, e2) -> 
      let (_, mem') = eval mem env e1 in
      eval mem' env e2
    | IF (cond_exp, then_exp, else_exp) ->
      let (cond_v, mem') = eval mem env cond_exp in
      if value_bool cond_v 
      then eval mem' env then_exp 
      else eval mem' env else_exp  
    | WHILE (cond_exp, loop_exp) -> 
      let (cond_v, mem') = eval mem env cond_exp in
      if value_bool cond_v
      then 
        let (_, mem'') = eval mem' env loop_exp in
        eval mem'' env e
      else (Unit, mem')
    | LETF (f_id, params_list, f, e) ->
      let proc = Proc (params_list, f, env) in
      let new_env = Env.bind env f_id proc in
      eval mem new_env e
    | CALLV (f_id, args_list) ->
      let (params_list, e', env') = lookup_env_proc env f_id in
      let args_length = List.length args_list and params_length = List.length params_list in
      if args_length = params_length 
      then 
        let update_fun = (fun (acc, m_acc) (param, arg) -> 
            let (v, mem') = eval m_acc env arg in
            ((param, v) :: acc, mem')) in
        let params_args = List.combine params_list args_list in
        let (alloc_list, m_n) = List.fold_left update_fun ([], mem) params_args in
        let update_fun' = fun (env, mem) (param, v) -> 
          let (l, mem') = Mem.alloc mem in
          (Env.bind env param (Addr l), Mem.store mem' l v) in
        let (final_env, final_mem) = List.fold_left update_fun' (env', m_n) alloc_list in
        eval final_mem (Env.bind final_env f_id (Proc (params_list, e', env'))) e'
      else raise (Error "InvalidArg")
    | CALLR (f_id, id_list) ->
      let (params_list, e, env') = lookup_env_proc env f_id in
      let id_length = List.length id_list and params_length = List.length params_list in
      if id_length = params_length 
      then 
        let update_fun = (fun env_acc (param, id) ->
            let l = lookup_env_loc env id in
            Env.bind env_acc param (Addr l)) in
        let params_id = List.combine params_list id_list in  
        let final_env = List.fold_left update_fun env' params_id in
        eval mem (Env.bind final_env f_id (Proc (params_list, e, env'))) e
      else raise (Error "InvalidArg")
    | RECORD [] -> (Unit, mem)
    | RECORD field_list -> 
      let update_fun = (fun (acc, m_acc) (id, exp) -> 
          let (v, mem') = eval m_acc env exp in
          ((id, v) :: acc, mem')) in
      let (alloc_list, m_n) = List.fold_left update_fun ([], mem) field_list in
      let update_fun' = (fun (record, mem) (id, v) -> 
          let (l, mem') = Mem.alloc mem in
          let bind record id l = (fun x -> if x = id then l else record x) in
          (bind record id l, Mem.store mem' l v)) in
      let (record, final_mem) = List.fold_left update_fun' ((fun _ -> raise (Error "TypeError : invalid field")), m_n) alloc_list in
      (Record record, final_mem)
    | FIELD (record_exp, id) ->
      let (record, mem') = eval mem env record_exp in
      let l = (value_record record) id in
      (Mem.load mem' l, mem')
    | ASSIGNF (record_exp, id, exp) ->
      let (record, mem') = eval mem env record_exp in
      let (v, mem'') = eval mem' env exp in
      let l = (value_record record) id in
      (v, Mem.store mem'' l v)

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
