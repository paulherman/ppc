# 1 "simp.mlp"
(* ppcpi/simp.mlp *)

open Keiko

(* |add_offset| -- add numeric offset to an address *)
let rec add_offset n =
  function
      (Node (LOCAL a, [])) -> (Node (LOCAL (a + n), []))
    | (Node (BINOP PlusA, [t1;  (Node (CONST a, []))])) -> add_offset (a+n) t1
    | t -> if n = 0 then t else (Node (BINOP PlusA, [t;  (Node (CONST n, []))]))

(* |exact_log2| -- return log2 of argument, or raise Not_found *)
let exact_log2 x =
  let rec loop y i =
    if y = 1 then i
    else if y mod 2 <> 0 then raise Not_found
    else loop (y/2) (i+1) in
  if x <= 0 then raise Not_found;
  loop x 0

(* |swap| -- find reverse operation or raise Not_found *)
let swap =
  function Plus -> Plus | Times -> Times | Eq -> Eq | Lt -> Gt 
    | Gt -> Lt | Leq -> Geq | Geq -> Leq | Neq -> Neq 
    | And -> And | Or -> Or
    | _ -> raise Not_found

(* |is_const| -- test if expression is a constant *)
let is_const = function (Node (CONST a, [])) -> true | _ -> false

(* |simp| -- simplify an expression tree at the root *)
let rec simp t =
  match t with
    (* Constant folding *)
      (Node (BINOP w, [(Node (CONST a, []));  (Node (CONST b, []))])) ->
        (Node (CONST (do_binop w a b), []))
    | (Node (MONOP w, [(Node (CONST a, []))])) ->
        (Node (CONST (do_monop w a), []))
    | (Node (JUMPC (w, lab), [(Node (CONST a, []));  (Node (CONST b, []))])) ->
        if do_binop w a b <> 0 then (Node (JUMP lab, [])) else (Node (NOP, []))

    (* Static bound checks *)
    | (Node (BOUND, [(Node (CONST k, []));  (Node (CONST b, []))])) -> 
        if 0 <= k && k < b then (Node (CONST k, [])) else t

    (* Simplifications -- mainly directed at addressing calculations *)
    | (Node (BINOP Plus, [t1;  (Node (CONST a, []))])) when a < 0 ->
        (Node (BINOP Minus, [t1;  (Node (CONST (-a), []))]))
    | (Node (BINOP Minus, [t1;  (Node (CONST a, []))])) when a < 0 -> 
        (Node (BINOP Plus, [t1;  (Node (CONST (-a), []))]))

    | (Node (BINOP PlusA, [t1;  (Node (CONST a, []))])) ->
        add_offset a t1
    | (Node (BINOP Times, [(Node (BINOP Times, [t1;  (Node (CONST a, []))]));  (Node (CONST b, []))])) ->
        simp (Node (BINOP Times, [t1;  (Node (CONST (a * b), []))]))
    | (Node (BINOP Times, [(Node (BINOP Plus, [t1;  (Node (CONST a, []))]));  (Node (CONST b, []))])) ->
        simp (Node (BINOP Plus, [
          simp (Node (BINOP Times, [t1;  (Node (CONST b, []))]));  
          (Node (CONST (a*b), []))]))
    | (Node (BINOP Times, [(Node (BINOP Minus, [t1;  (Node (CONST a, []))]));  (Node (CONST b, []))])) ->
        simp (Node (BINOP Minus, [
          simp (Node (BINOP Times, [t1;  (Node (CONST b, []))]));  
          (Node (CONST (a*b), []))]))
    | (Node (BINOP PlusA, [t1;  (Node (BINOP Plus, [t2;  t3]))])) ->
        simp (Node (BINOP PlusA, [simp (Node (BINOP PlusA, [t1;  t2]));  t3]))
    | (Node (BINOP PlusA, [t1;  (Node (BINOP Minus, [t2;  (Node (CONST n, []))]))])) ->
        simp (Node (BINOP PlusA, [simp (Node (BINOP PlusA, [t1;  t2]));  (Node (CONST (-n), []))]))
    | (Node (BINOP Times, [t1;  (Node (CONST 1, []))])) -> t1
    | (Node (BINOP Times, [t1;  (Node (CONST n, []))])) when n > 0 -> 
        (try 
            let k = exact_log2 n in
            (Node (BINOP Lsl, [t1;  (Node (CONST k, []))]))
          with Not_found -> t)
    | (Node (BINOP Plus, [t1;  (Node (CONST 0, []))])) -> t1
    | (Node (BINOP Minus, [t1;  (Node (CONST 0, []))])) -> t1

    (* Swap operands to put constant on right *)
    | (Node (BINOP w, [(Node (CONST a, []));  t2])) ->
        if is_const t2 || not (Util.can swap w) then t else
          simp (Node (BINOP (swap w), [t2;  (Node (CONST a, []))]))
    | (Node (JUMPC (w, lab), [(Node (CONST a, []));  t2])) ->
        if is_const t2 then t else
          simp (Node (JUMPC (swap w, lab), [t2;  (Node (CONST a, []))]))

    | _ -> t

(* |simplify| -- recursively simplify an expression *)
let rec simplify (Node (x, ts)) = simp (Node (x, (List.map simplify ts)))

(* |optimise| -- simplify a procedure body *)
let optimise prog = 
  List.map simplify prog

