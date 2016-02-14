# 1 "tran.mlp"
(* ppcpi/tran.mlp *)

open Keiko
open Target
open Regs
open Print
open Util

let debug = ref 0

(* |release| -- release any register used by a value *)
let release =
  function
      Register reg -> release_reg reg
    | Index (reg, off) -> release_reg reg
    | Index2 (r1, r2, n) -> release_reg r1; release_reg r2
    | _ -> ()

let fix_reg r = Register (get_reg (reg_of r))

(* |gen_reg| -- emit instruction with result in a register *)
let gen_reg op rands =
  List.iter release (List.tl rands);
  let r' = fix_reg (List.hd rands) in
  emit op (r' :: List.tl rands);
  r'

(* |gen| -- emit an instruction *)
let gen op rands =
  List.iter release rands;
  emit op rands

(* |gen_move| -- move value to specific register *)
let gen_move dst src =
  if reg_of dst = R_any || reg_of dst = R_temp || dst = src then
    src
  else
    gen_reg "mov" [dst; src]


(* Tests for fitting in various immediate fields *)

(* |fits_offset| -- test for fitting in offset field of address *)
let fits_offset x = (-4096 < x && x < 4096)

(* |fits_immed| -- test for fitting in immediate field *)
let fits_immed x =
  (* A conservative approximation, using shifts instead of rotates *)
  let rec reduce r =
    if r land 3 <> 0 then r else reduce (r lsr 2) in
  x = 0 || x > 0 && reduce x < 256

(* |fits_move| -- test for fitting in immediate move *)
let fits_move x = fits_immed x || fits_immed (lnot x)

(* |fits_add| -- test for fitting in immediate add *)
let fits_add x = fits_immed x || fits_immed (-x)


(* |line| -- current line number *)
let line = ref 0

let argregs = Stack.create ()

let keep_arg v =
  Stack.push (reg_of v) argregs


(* The main part of the code generator consists of a family of functions
   e_X t, each generating code for a tree t, leaving the value in
   a register, as an operand that can be used in another instruction, etc. *)

let anyreg = Register R_any

(* |e_reg| -- evaluate expression with result in specified register *)
let rec e_reg r t =
  (* returns |Register| *)

  (* Binary operation *)
  let binary op t1 t2 =
    let v1 = e_reg anyreg t1 in
    let v2 = e_rand t2 in
    gen_reg op [r; v1; v2]

  (* Unary operation *)
  and unary op t1 =
    let v1 = e_reg anyreg t1 in
    gen_reg op [r; v1]

  (* Comparison with boolean result *)
  and compare op t1 t2 =
    let v1 = e_reg anyreg t1 in
    let v2 = e_rand t2 in
    release v1; release v2;
    let rr = fix_reg r in
    emit "cmp" [v1; v2];
    emit "mov" [rr; Const 0];
    emit op [rr; Const 1];
    rr in

  match t with
      (Node (CONST k, [])) when fits_move k -> 
        gen_reg "mov" [r; Const k]
    | (Node (CONST k, [])) ->
        gen_reg "ldr" [r; Literal ("", k)]
    | (Node (LOCAL 0, [])) ->
        gen_move r (Register R_fp)
    | (Node (LOCAL n, [])) when fits_add n ->
        gen_reg "add" [r; Register R_fp; Const n]
    | (Node (LOCAL n, [])) ->
        emit "ldr" [Register R_ip; Literal ("", n)];
        gen_reg "add" [r; Register R_fp; Register R_ip]
    | (Node (GLOBAL x, [])) ->
        gen_reg "ldr" [r; Literal (x, 0)]
    | (Node (TEMP n, [])) ->
        gen_move r (Register (Regs.use_temp n))
    | (Node ((LOADW|LOADC), [(Node (REGVAR i, []))])) ->
        let rv = List.nth stable i in
        reserve_reg rv; gen_move r (Register rv)
    | (Node (LOADW, [t1])) -> 
        let v1 = e_addr t1 in
        gen_reg "ldr" [r; v1]
    | (Node (LOADC, [t1])) -> 
        let v1 = e_addr t1 in
        gen_reg "ldrb" [r; v1]
    | (Node (PCALL _, [_])) ->
        e_call t;
        reserve_reg (R 0); gen_move r (Register (R 0))

    | (Node (MONOP Uminus, [t1])) -> unary "neg" t1
    | (Node (MONOP Not, [t1])) -> 
        let v1 = e_reg anyreg t1 in
        gen_reg "eor" [r; v1; Const 1]
    | (Node (MONOP BitNot, [t1])) -> unary "mvn" t1

    | (Node (BINOP PlusA, [t1;  (Node (CONST n, []))])) when fits_add n ->
        (* Allow add for negative constants *)
        let v1 = e_reg anyreg t1 in
        gen_reg "add" [r; v1; Const n]
    | (Node (BINOP PlusA, [t1;  t2])) -> binary "add" t1 t2

    | (Node (BINOP Plus, [t1;  t2])) -> binary "add" t1 t2
    | (Node (BINOP Minus, [t1;  t2])) -> binary "sub" t1 t2
    | (Node (BINOP And, [t1;  t2])) -> binary "and" t1 t2
    | (Node (BINOP Or, [t1;  t2])) -> binary "orr" t1 t2
    | (Node (BINOP Lsl, [t1;  t2])) -> binary "lsl" t1 t2
    | (Node (BINOP Lsr, [t1;  t2])) -> binary "lsr" t1 t2
    | (Node (BINOP Asr, [t1;  t2])) -> binary "asr" t1 t2
    | (Node (BINOP BitAnd, [t1;  t2])) -> binary "and" t1 t2
    | (Node (BINOP BitOr, [t1;  t2])) -> binary "orr" t1 t2

    | (Node (BINOP Times, [t1;  t2])) ->
        (* The mul instruction needs both operands in registers *)
        let v1 = e_reg anyreg t1 in
        let v2 = e_reg anyreg t2 in
        gen_reg "mul" [r; v1; v2]

    | (Node (BINOP Eq, [t1;  t2])) -> compare "moveq" t1 t2
    | (Node (BINOP Neq, [t1;  t2])) -> compare "movne" t1 t2
    | (Node (BINOP Gt, [t1;  t2])) -> compare "movgt" t1 t2
    | (Node (BINOP Geq, [t1;  t2])) -> compare "movge" t1 t2
    | (Node (BINOP Lt, [t1;  t2])) -> compare "movlt" t1 t2
    | (Node (BINOP Leq, [t1;  t2])) -> compare "movle" t1 t2

    | (Node (BOUND, [t1;  t2])) ->
        let v1 = e_reg r t1 in
        let v2 = e_rand t2 in
        release v2;
        emit "cmp" [v1; v2];
        emit "ldrhs" [Register (R 0); Literal ("", !line)];
        emit "blhs" [Global "check"];
        v1

    | (Node (NCHECK, [t1])) ->
        let v1 = e_reg r t1 in
        emit "cmp" [v1; Const 0];
        emit "ldreq" [Register (R 0); Literal ("", !line)];
        emit "bleq" [Global "nullcheck"];
        v1

    | (Node (w, args)) ->
        failwith (sprintf "eval $" [fInst w])

(* |e_rand| -- evaluate to form second operand *)
and e_rand =
  (* returns |Const| or |Register| *)
  function
      (Node (CONST k, [])) when fits_immed k -> Const k
    | t -> e_reg anyreg t 

(* |e_addr| -- evaluate to form an address for ldr or str *)
and e_addr =
  (* returns |Index| *)
  function
      (Node (LOCAL n, [])) when fits_offset n ->
        Index (R_fp, n) 
    | (Node (BINOP PlusA, [t1;  (Node (CONST n, []))])) when fits_offset n ->
        let v1 = e_reg anyreg t1 in
        Index (reg_of v1, n)
    | t ->
        let v1 = e_reg anyreg t in
        Index (reg_of v1, 0)

(* |e_call| -- execute procedure call *)
and e_call t =
  (* Spill any temps that weren't spilled before *)
  spill_temps volatile;

  (* Call the function *)
  begin match t with
      (Node (PCALL n, [(Node (GLOBAL f, []))])) -> 
        gen "bl" [Global f]
    | (Node (PCALL n, [t1])) -> 
        let v1 = e_reg anyreg t1 in
        gen "blx" [v1]
    | (Node (w, args)) -> 
        failwith (sprintf "e_call $" [fInst w])
  end;

  (* Release argument registers *)
  Stack.iter release_reg argregs;
  Stack.clear argregs

(* |e_stmt| -- generate code to execute a statement *)
let e_stmt t =

(* Conditional jump *)
  let condj op lab t1 t2 =
    let v1 = e_reg anyreg t1 in
    let v2 = e_rand t2 in
    gen "cmp" [v1; v2];
    gen op [Label lab] in

  match t with
      (Node (DEFTMP n, [t1])) ->
        let v1 = e_reg (Register R_temp) t1 in
        Regs.def_temp n (reg_of v1)

    | (Node ((STOREW|STOREC), [t1;  (Node (REGVAR i, []))])) ->
        let rv = List.nth stable i in
        release (e_reg (Register rv) t1)
    | (Node (STOREW, [t1;  t2])) -> 
        let v1 = e_reg anyreg t1 in
        let v2 = e_addr t2 in
        gen "str" [v1; v2]
    | (Node (STOREC, [t1;  t2])) -> 
        let v1 = e_reg anyreg t1 in
        let v2 = e_addr t2 in
        gen "strb" [v1; v2]

    | (Node (PCALL _, [_])) -> e_call t

    | (Node (RESULTW, [t1])) ->
        release (e_reg (Register (R 0)) t1)

    | (Node (LABEL lab, [])) -> emit_lab lab

    | (Node (JUMP lab, [])) -> gen "b" [Label lab]

    | (Node (JUMPC (Eq, lab), [t1;  t2])) -> condj "beq" lab t1 t2
    | (Node (JUMPC (Lt, lab), [t1;  t2])) -> condj "blt" lab t1 t2
    | (Node (JUMPC (Gt, lab), [t1;  t2])) -> condj "bgt" lab t1 t2
    | (Node (JUMPC (Leq, lab), [t1;  t2])) -> condj "ble" lab t1 t2
    | (Node (JUMPC (Geq, lab), [t1;  t2])) -> condj "bge" lab t1 t2
    | (Node (JUMPC (Neq, lab), [t1;  t2])) -> condj "bne" lab t1 t2

    | (Node (JCASE (table, deflab), [t1])) ->
        (* This jump table code exploits the fact that on ARM, reading
           the pc gives a value 8 bytes beyond the current instruction,
           so in the ldrlo instruction below, pc points to the branch
           table itself. *)
        let v1 = e_reg anyreg t1 in
        emit "cmp" [v1; Const (List.length table)];
        gen "ldrlo" [Register R_pc; Index2 (R_pc, reg_of v1, 2)];
        gen "b" [Label deflab];
        List.iter (fun lab -> emit ".word" [Label lab]) table

    | (Node (ARG i, [(Node (TEMP k, []))])) when i < 4 ->
        (* Avoid annoying spill and reload if the value is a temp
           already in the correct register: e.g. in f(g(x)). *)
        let r = R i in
        let r1 = Regs.use_temp k in
        spill_temps [r];
        keep_arg (gen_move (Register r) (Register r1))
    | (Node (ARG i, [t1])) when i < 4 ->
        let r = R i in
        spill_temps [r];
        keep_arg (e_reg (Register r) t1)
    | (Node (ARG i, [t1])) when i >= 4 ->
        need_stack (4*i-12);
        let v1 = e_reg anyreg t1 in
        gen "str" [v1; Index (R_sp, 4*i-16)]

    | (Node (SLINK, [(Node (CONST 0, []))])) -> ()
    | (Node (SLINK, [t1])) ->
        let r = R 4 in
        spill_temps [r];
        keep_arg (e_reg (Register r) t1)

    | (Node (w, ts)) -> 
        failwith (sprintf "e_stmt $" [fInst w])

(* |process| -- generate code for a statement, or note a line number *)
let process =
  function
      (Node (LINE n, [])) ->
        if !line <> n then
          emit_comment (Source.get_line n);
        line := n
    | t ->
        if !debug > 0 then emit_tree t;
        e_stmt t;
        if !debug > 1 then emit_comment (Regs.dump_regs ())

(* |translate| -- translate a procedure body *)
let translate lab level nargs nregv fsize code =
  Target.start_proc lab level nargs fsize;
  Regs.get_regvars nregv;
  (try List.iter process code with exc -> 
    (* Code generation failed, but let's see how far we got *)
    Target.flush_proc (); raise exc);
  Target.end_proc ()
