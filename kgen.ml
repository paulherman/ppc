# 1 "kgen.mlp"
(* ppc/kgen.ml *)

open Dict
open Tree
open Mach
open Keiko
open Lexer
open Print

let boundchk = ref false
let optlevel = ref 0
let debug = ref 0

(* |level| -- nesting level of current procedure *)
let level = ref 0

(* |retlab| -- label to return from current procedure *)
let retlab = ref nolab

(* |size_of| -- calculate size of type *)
let size_of t = t.t_rep.r_size

(* |count_of| -- calculate number of parameter words *)
let count_of t = if t.t_rep.r_size = 0 then 0 else 1

(* |is_const| -- test if expression is a constant *)
let is_const e = (e.e_value <> None)

(* |get_value| -- get constant value or fail *)
let get_value e =
  match e.e_value with
      Some v -> v
    | None -> failwith "get_value"

(* |arg_size| -- compute size of argument *)
let arg_size f =
  match f.d_kind with PParamDef -> 2 | _ -> 1

(* |line_number| -- compute line number of variable for bound check *)
let rec line_number v =
  match v.e_guts with
      Variable x -> x.x_line
    | Sub (a, i) -> line_number a
    | Select (r, x) -> x.x_line
    | Deref p -> line_number p
    | _ -> failwith "line_number"

(* |addr_size| -- size of address *)
let addr_size = addr_rep.r_size

(* |schain| -- code to follow N links of static chain *)
let rec schain n =
  if n = 0 then
    (Node (LOCAL 0, []))
  else if n = 1 then
    (Node (LOADW, [(Node (LOCAL stat_link, []))]))
  else
    (Node (LOADW, [(Node (BINOP PlusA, [schain (n-1);  (Node (CONST stat_link, []))]))]))

(* |address| -- code to push address of an object *)
let address d =
  match d.d_addr with
      Global g -> (Node (GLOBAL g, []))
    | Local off -> 
        if d.d_level = !level then 
          (Node (LOCAL off, []))
        else
          (Node (BINOP PlusA, [schain (!level - d.d_level);  (Node (CONST off, []))]))
    | Register i ->
        (Node (REGVAR i, []))
    | Nowhere -> 
        failwith (sprintf "address $" [fId d.d_tag])

(* |gen_closure| -- two trees for a (code, envt) pair *)
let gen_closure d =
  match d.d_kind with
      ProcDef ->
        (address d,
          if d.d_level = 0 then (Node (CONST 0, [])) else schain (!level - d.d_level))
    | PParamDef ->
        ((Node (LOADW, [address d])),
          (Node (LOADW, [(Node (BINOP PlusA, [address d;  (Node (CONST addr_size, []))]))])))
    | _ -> failwith "missing closure"

let rec numargs i =
  function
      [] -> []
    | (x::xs) -> (Node (ARG i, [x])) :: numargs (i+1) xs

(* |libcall| -- code for library call *)
let libcall lab n args rtype =
  (Node (PCALL n, ((Node (GLOBAL lab, [])) :: numargs 0 args)))

(* |gen_copy| -- generate code to copy a fixed-size chunk *)
let gen_copy dst src n =
  libcall "memcpy" 3 [dst; src; (Node (CONST n, []))] voidtype

(* |gen_addr| -- code for the address of a variable *)
let rec gen_addr v = 
  match v.e_guts with
      Variable x ->
        let d = get_def x in
        begin
          match d.d_kind with
              VarDef | RegDef ->
                address d
            | VParamDef ->
                (Node (LOADW, [address d]))
            | CParamDef ->
                if scalar d.d_type || is_pointer d.d_type then 
                  address d
                else
                  (Node (LOADW, [address d]))
            | StringDef ->
                address d
            | _ -> 
                failwith "load_addr"
        end
    | Sub (a, i) ->
        let bound_check t =
          if not !boundchk then t else (Node (BOUND, [t;  (Node (CONST (bound a.e_type), []))])) in
        (Node (BINOP PlusA, [
          gen_addr a; 
          (Node (BINOP Times, [bound_check (gen_expr i);  (Node (CONST (size_of v.e_type), []))]))]))
    | Select (r, x) ->
        let d = get_def x in
        (Node (BINOP PlusA, [gen_addr r;  (Node (CONST (offset_of d), []))]))
    | Deref p ->
        let null_check t =
          if not !boundchk then t else (Node (NCHECK, [t])) in
        null_check (gen_expr p)
    | String (lab, n) -> (Node (GLOBAL lab, []))
    | _ -> failwith "gen_addr"

(* |gen_expr| -- tree for the value of an expression *)
and gen_expr e =
  match e.e_value with
      Some v -> 
        (Node (CONST v, []))
    | None -> 
        begin
          match e.e_guts with
              Variable _ | Sub _ | Select _ | Deref _ ->
                let ld = if size_of e.e_type = 1 then LOADC else LOADW in
                (Node (ld, [gen_addr e]))
            | Monop (w, e1) ->
                (Node (MONOP w, [gen_expr e1]))
            | Binop (Div, e1, e2) ->
                libcall "int_div" 2 [gen_expr e1; gen_expr e2] integer
            | Binop (Mod, e1, e2) ->
                libcall "int_mod" 2 [gen_expr e1; gen_expr e2] integer
            | Binop (w, e1, e2) ->
                (Node (BINOP w, [gen_expr e1;  gen_expr e2]))
            | FuncCall (p, args) -> 
                gen_call p args
            | _ -> failwith "gen_expr"
        end

(* |gen_call| -- generate code to call a procedure *)
and gen_call x args =
  let d = get_def x in
  match d.d_kind with
      LibDef q ->
        gen_libcall q args d.d_type
    | _ ->
        let p = get_proc d.d_type in
        let (fn, sl) = gen_closure d in
        let args = List.concat (List.map2 gen_arg p.p_fparams args) in
        (Node (PCALL p.p_pcount, (fn :: (Node (SLINK, [sl])) :: numargs 0 args)))

(* |gen_arg| -- generate code for a procedure argument *)
and gen_arg f a = 
  match f.d_kind with
      CParamDef ->
        if scalar f.d_type || is_pointer f.d_type then 
          [gen_expr a]
        else 
          [gen_addr a]
    | VParamDef ->
        [gen_addr a]
    | PParamDef ->
        begin
          match a.e_guts with 
              Variable x -> 
                let (fn, sl) = gen_closure (get_def x) in [fn; sl]
            | _ -> 
                failwith "bad funarg"
        end
    | _ -> failwith "bad arg"

(* |gen_libcall| -- generate code to call a built-in procedure *)
and gen_libcall q args rtype =
  match (q.q_id, args) with
      (ChrFun, [e]) -> gen_expr e
    | (OrdFun, [e]) -> gen_expr e
    | (PrintString, [e]) ->
        libcall "print_string" 2 
          [gen_addr e; (Node (CONST (bound e.e_type), []))] voidtype
    | (ReadChar, [e]) ->
        libcall "read_char" 1 [gen_addr e] voidtype
    | (NewProc, [e]) ->
        let size = size_of (base_type e.e_type) in
        libcall "new" 2 [gen_addr e; (Node (CONST size, []))] voidtype
    | (ArgvProc, [e1; e2]) ->
        libcall "argv" 2 [gen_expr e1; gen_addr e2] voidtype
    | (OpenIn, [e]) ->
        libcall "open_in" 1 [gen_addr e] voidtype
    | (Operator op, [e1]) ->
        (Node (MONOP op, [gen_expr e1]))
    | (Operator op, [e1; e2]) ->
        (Node (BINOP op, [gen_expr e1;  gen_expr e2]))
    | (_, _) ->
        let proc = sprintf "$" [fLibId q.q_id] in
        libcall proc (List.length args)
          (List.map gen_expr args) rtype

(* |gen_cond| -- generate code to branch on a condition *)
let rec gen_cond tlab flab test =
  match test.e_value with
      Some v ->
        if v <> 0 then (Node (JUMP tlab, [])) else (Node (JUMP flab, []))
    | None ->
        begin
          match test.e_guts with
              Monop (Not, e) ->
                gen_cond flab tlab e
            | Binop (Or, e1, e2) ->
                let l1 = label () in
                (Node (SEQ, [gen_cond tlab l1 e1; 
                  (Node (LABEL l1, []));  gen_cond tlab flab e2]))
            | Binop (And, e1, e2) ->
                let l1 = label () in
                (Node (SEQ, [gen_cond l1 flab e1; 
                  (Node (LABEL l1, []));  gen_cond tlab flab e2]))
            | Binop ((Eq | Neq | Lt | Leq | Gt | Geq) as w, e1, e2) ->
                (Node (SEQ, [(Node (JUMPC (w, tlab), [gen_expr e1;  gen_expr e2])); 
                  (Node (JUMP flab, []))]))
            | _ ->
                (Node (SEQ, [(Node (JUMPC (Neq, tlab), [gen_expr test;  (Node (CONST 0, []))])); 
                  (Node (JUMP flab, []))]))
        end

let gen_jtable sel tab0 deflab =
  if tab0 = [] then (Node (JUMP deflab, [])) else begin
    let table = List.sort (fun (v1, l1) (v2, l2) -> compare v1 v2) tab0 in
    let lob = fst (List.hd table) in
    let rec tab u qs =
      match qs with
          [] -> []
        | (v, l) :: rs -> 
            if u = v then l :: tab (v+1) rs else deflab :: tab (u+1) qs in
    (Node (JCASE (tab lob table, deflab), [(Node (BINOP Minus, [sel;  (Node (CONST lob, []))]))]))
  end

(* |gen_stmt| -- generate code for a statement *)
let rec gen_stmt s = 
  let code =
    match s.s_guts with
        Skip -> (Node (NOP, []))
      | Seq ss -> (Node (SEQ, (List.map gen_stmt ss)))
      | Assign (v, e) ->
          if scalar v.e_type || is_pointer v.e_type then begin
            let st = if size_of v.e_type = 1 then STOREC else STOREW in
            (Node (st, [gen_expr e;  gen_addr v]))
          end else begin
            gen_copy (gen_addr v) (gen_addr e) (size_of v.e_type)
          end
      | ProcCall (p, args) ->
          gen_call p args
      | Return res ->
          begin
            match res with
                Some e -> (Node (SEQ, [(Node (RESULTW, [gen_expr e]));  (Node (JUMP !retlab, []))]))
              | None -> (Node (JUMP !retlab, []))
          end
      | IfStmt (test, thenpt, elsept) ->
          let l1 = label () and l2 = label () and l3 = label() in
          (Node (SEQ, [gen_cond l1 l2 test; 
            (Node (LABEL l1, []));  gen_stmt thenpt;  (Node (JUMP l3, [])); 
            (Node (LABEL l2, []));  gen_stmt elsept;  (Node (LABEL l3, []))]))
      | WhileStmt (test, body) ->
          let l1 = label () and l2 = label () and l3 = label() in
          (Node (SEQ, [(Node (JUMP l2, []));  (Node (LABEL l1, []));  gen_stmt body; 
            (Node (LABEL l2, []));  (Node (LINE s.s_line, []));  gen_cond l1 l3 test;  (Node (LABEL l3, []))]))
      | RepeatStmt (body, test) ->
          let l1 = label () and l2 = label () in
          (Node (SEQ, [(Node (LABEL l1, []));  gen_stmt body;  
            gen_cond l2 l1 test;  (Node (LABEL l2, []))]))
      | ForStmt (var, lo, hi, body) ->
          (* For simplicity, this code re-evaluates hi on each iteration *)
          let l1 = label () and l2 = label () in
          (Node (SEQ, [(Node (STOREW, [gen_expr lo;  gen_addr var]));  (Node (JUMP l2, [])); 
            (Node (LABEL l1, []));  gen_stmt body;  (Node (LINE s.s_line, [])); 
            (Node (STOREW, [(Node (BINOP Plus, [gen_expr var;  (Node (CONST 1, []))]));  gen_addr var])); 
            (Node (LABEL l2, []));  (Node (JUMPC (Leq, l1), [gen_expr var;  gen_expr hi]))])) 
      | CaseStmt (sel, arms, deflt) ->
          let deflab = label () and donelab = label () in
          let labs = List.map (function x -> label ()) arms in
          let get_val (v, body) = get_value v in
          let table = List.combine (List.map get_val arms) labs in
          let gen_case lab (v, body) =
            (Node (SEQ, [(Node (LABEL lab, []));  gen_stmt body;  (Node (JUMP donelab, []))])) in
          (Node (SEQ, [gen_jtable (gen_expr sel) table deflab; 
            (Node (SEQ, (List.map2 gen_case labs arms))); 
            (Node (LABEL deflab, []));  gen_stmt deflt; 
            (Node (LABEL donelab, []))])) in
  (Node (SEQ, [(Node (LINE s.s_line, []));  code]))

(* unnest -- move procedure calls to top level *)
let unnest code =
  let rec do_tree =
    function
        (Node (PCALL n, args)) ->
          let t = Regs.new_temp 2 in
          (Node (AFTER, [
            (Node (DEFTMP t, [(Node (PCALL n, (List.map do_tree args)))]));  
            (Node (TEMP t, []))]))
      | (Node (w, args)) ->
          (Node (w, (List.map do_tree args))) in
  let do_root =
    function (Node (op, args)) -> (Node (op, (List.map do_tree args))) in
  Keiko.canon (Node (SEQ, (List.map do_root code)))

let show label code =
  if !debug > 0 then begin
    printf "$$:\n" [fStr Mach.comment; fStr label];
    List.iter (Keiko.print_optree Mach.comment) code;
    printf "\n" []
  end;
  code

(* |do_proc| -- generate code for a procedure and pass to the back end *)
let do_proc lab lev nargs fsize nregv body =
  level := lev+1;
  retlab := label ();
  let code0 = 
    show "Initial code" (Keiko.canon (Node (SEQ, [gen_stmt body;  (Node (LABEL !retlab, []))]))) in
  Regs.init ();
  let code1 = if !optlevel < 1 then code0 else
      show "After simplification" (Jumpopt.optimise (Simp.optimise code0)) in
  let code2 = if !optlevel < 2 then 
      show "After unnesting" (unnest code1) 
    else
      show "After sharing" (Share.traverse code1) in
  Tran.translate lab lev nargs nregv fsize (flatten code2)

(* |gen_proc| -- translate a procedure, ignore other declarations *)
let rec gen_proc = 
  function
      ProcDecl (Heading (x, _, _), Block (locals, body, fsize, nregv)) ->
        let d = get_def x in
        let p = get_proc d.d_type in
        begin
          match d.d_addr with 
              Global lab ->
                let line = Source.get_line x.x_line in
                printf "$$\n" [fStr Mach.comment; fStr line];
                do_proc lab d.d_level p.p_pcount !fsize !nregv body;
                gen_procs locals
            | _ -> failwith "gen_proc"
        end
    | _ -> ()

(* |gen_procs| -- generate code for the procedures in a block *)
and gen_procs ds = List.iter gen_proc ds

(* |gen_global| -- generate declaration for global variable *)
let gen_global d =
  match d.d_kind with
      VarDef ->
        (match d.d_addr with
            Global lab -> 
              Target.emit_global lab (size_of d.d_type)
          | _ -> failwith "gen_global")
    | _ -> ()

(* |translate| -- generate code for the whole program *)
let translate (Prog (Block (globals, main, _, _), glodefs)) =
  Target.preamble ();
  gen_procs globals;
  do_proc "pmain" 0 0 0 0 main;
  List.iter gen_global !glodefs;
  List.iter (fun (lab, s) -> Target.emit_string lab s) (string_table ());
  Target.postamble ()

