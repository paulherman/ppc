# 1 "keiko.mlp"
(* ppc/keiko.ml *)

open Print
open Util

(* |symbol| -- global symbols *)
type symbol = string

type codelab = int

let nolab = -1

(* |lab| -- last used code label *)
let lab = ref 0

(* |label| -- allocate a code label *)
let label () = incr lab; !lab

(* |fLab| -- format a code label for printf *)
let fLab n = fMeta "$" [fNum n]

let nosym = "*nosym*"

let gensym () = sprintf "g$" [fLab (label ())]

(* |op| -- type of picoPascal operators *)
type op =
  | Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | And | Or | Not | PlusA | Lsl
  | Lsr | Asr | BitAnd | BitOr | BitNot

(* |inst| -- type of intermediate instructions *)
type inst =
    CONST of int                (* Constant (value) *)
  | GLOBAL of symbol            (* Constant (symbol, offset) *)
  | LOCAL of int                (* Local address (offset) *)
  | REGVAR of int               (* Register (index) *)
  | LOADC                       (* Load char *)
  | LOADW                       (* Load word *)
  | STOREC                      (* Store char *)
  | STOREW                      (* Store word *)
  | ARG of int                  (* Pass argument (index) *)
  | SLINK                       (* Pass static link *)
  | PCALL of int                (* Call procedure (nparams) *)
  | RESULTW                     (* Procedure result *)
  | MONOP of op                 (* Perform unary operation (op) *)
  | BINOP of op                 (* Perform binary operation (op) *)
  | BOUND                       (* Array bound check *)
  | NCHECK                      (* Null pointer check *)
  | LABEL of codelab            (* Set code label *)
  | JUMP of codelab             (* Unconditional branch (dest) *)
  | JUMPC of op * codelab       (* Conditional branch (cond, dest) *)
  | JCASE of codelab list * codelab (* Jump table *)

  (* Extra instructions *)
  | LINE of int                 (* Line number *)
  | NOP
  | SEQ
  | AFTER                       (* Expression with side effect *)
  | DEFTMP of int
  | TEMP of int                 (* Temporary *)

(* Operator trees *)
type optree = inst list_tree

let int_of_bool b = if b then 1 else 0

(* |do_monop| -- evaluate unary operators *)
let do_monop w x =
  match w with
      Uminus -> - x
    | Not -> if x <> 0 then 0 else 1
    | BitNot -> lnot x
    | _ -> failwith "do_monop"

(* |do_binop| -- evaluate binary operators *)
let do_binop w x y =
  match w with
      Plus -> x + y
    | Minus -> x - y
    | Times -> x * y
    | Div -> x / y
    | Mod -> x mod y
    | Eq -> int_of_bool (x = y)
    | Lt -> int_of_bool (x < y)
    | Gt -> int_of_bool (x > y)
    | Leq -> int_of_bool (x <= y)
    | Geq -> int_of_bool (x >= y)
    | Neq -> int_of_bool (x <> y)
    | And -> if x <> 0 then y else 0
    | Or -> if x <> 0 then 1 else y
    | BitAnd -> x land y
    | BitOr -> x lor y
    | Lsl -> x lsl y
    | Lsr -> x lsr y
    | Asr -> x asr y
    | _ -> failwith "do_binop"

(* |negate| -- negation of a comparison *)
let negate = 
  function Eq -> Neq | Neq -> Eq | Lt  -> Geq
    | Leq -> Gt | Gt  -> Leq | Geq -> Lt
    | _ -> failwith "negate"

let rec canon_app t us =
  match t with
      (Node (SEQ, ts)) -> List.fold_right canon_app ts us
    | (Node (NOP, [])) -> us
    | (Node (LINE n, [])) -> if n = 0 then us else (Node (LINE n, [])) :: set_line n us
    | _ -> effects t (result t :: us)

and set_line n ts =
  match ts with 
      [] -> []
    | (Node (LINE m, [])) :: us -> if n <> m then ts else us
    | u :: us -> u :: set_line n us

and effects t us =
  match t with
      (Node (AFTER, [t1;  t2])) -> canon_app t1 (effects t2 us)
    | (Node (w, ts)) -> List.fold_right effects ts us

and result =
  function
      (Node (AFTER, [t1;  t2])) -> result t2
    | (Node (w, ts)) -> (Node (w, (List.map result ts)))

let canon t = canon_app t []

let flat =
  function
      (Node (PCALL n, (fn::args))) -> 
        List.rev args @ [(Node (PCALL n, [fn]))]
    | (Node (DEFTMP k, [(Node (PCALL n, (fn::args)))])) ->
        List.rev args @ [(Node (DEFTMP k, [(Node (PCALL n, [fn]))]))]
    | t -> [t]

let flatten ts = List.concat (List.map flat ts)

let string_of_op op = match op with
    | Plus -> "Plus" 
    | Minus -> "Minus"
    | Times -> "Times"
    | Div -> "Div"
    | Mod -> "Mod"
    | Eq -> "Eq"
    | Uminus -> "UMinus"
    | Lt -> "Lt"
    | Gt -> "Gt"
    | Leq -> "Leq"
    | Geq -> "Geq"
    | Neq -> "Neq"
    | And -> "And"
    | Or -> "or"
    | Not -> "Not"
    | PlusA -> "PlusA"
    | Lsl-> "Lsl"
    | Lsr -> "Lsr"
    | Asr -> "Asr"
    | BitAnd -> "BitAnd"
    | BitOr -> "BitOr"
    | BitNot-> "BitNot"
    
let string_of_inst instr = match instr with
    | CONST k -> "CONST " ^ (string_of_int k)
    | GLOBAL g -> "GLOBAL " ^ g
    | LOCAL l -> "LOCAL " ^ (string_of_int l)
    | REGVAR r -> "REGVAR " ^ (string_of_int r)
    | LOADC -> "LOADC"
    | LOADW -> "LOADW"
    | STOREC -> "STOREC"
    | STOREW -> "STOREW"
    | ARG a -> "ARG " ^ (string_of_int a)
    | SLINK -> "SLINK"
    | PCALL n -> "PCALL " ^ (string_of_int n)
    | RESULTW -> "RESULTW"
    | MONOP o -> "MONOP " ^ (string_of_op o)
    | BINOP o -> "BINOP " ^ (string_of_op o)
    | BOUND -> "BOUND"
    | NCHECK -> "NCHECK"
    | LABEL l -> "LABEL " ^ (string_of_int l)
    | JUMP l -> "JUMP " ^ (string_of_int l)
    | JUMPC (o, l) -> "JUMPC " ^ (string_of_int l) ^ " " ^ (string_of_op o)
    | JCASE (ls, l) -> "JCASE " ^ (string_of_int l) ^ " (" ^ (String.concat ", " (List.map string_of_int ls)) ^ ")"
    | TEMP l -> "TEMP " ^ (string_of_int l)
    | DEFTMP l -> "DEFTMP " ^ (string_of_int l) 
    | NOP -> "NOP"
    | LINE line -> "LINE " ^ string_of_int line 
    | _ -> "UNKINSTR"
    
let fOp w = fStr (string_of_op w)

let fInst instr = fStr (string_of_inst instr)

let rec print_optree prefix tree =  print_optree' prefix 0 tree
and print_optree' prefix depth tree =
    let indent = String.concat "" (copy depth "  ") in
    match tree with
        | Node (instr, children) ->
            print_endline (prefix ^ indent ^ string_of_inst instr);
            List.iter (print_optree' prefix (depth + 1)) children
