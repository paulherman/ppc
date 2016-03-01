open Gen
open Keiko
open Util
open Regalloc

type arm_part =
      Lit of string
    | Out
    | In
    | VReg of int
    | PReg of string
    
type arm_instr_tree =
      ArmInstrNode of arm_part list * arm_instr_tree list
    | ArmInstrPart of arm_part list * arm_instr_tree list
    | ArmDefTemp of int * arm_instr_tree
    | ArmUseTemp of int
    
type arm_instr = arm_part list

type global = symbol * int

type proc = symbol * int * int * int * int * optree list

type str_literal = symbol * string

let string_of_arm_part part = match part with
    | Lit lit -> lit
    | In -> "$IN"
    | Out -> "$OUT"
    | VReg reg_num -> "v" ^ string_of_int reg_num
    | PReg reg -> reg
    
let print_instr i = match i with
    | (instr, out_reg, in_regs) -> print_endline (String.concat "" (List.map string_of_arm_part instr))
  
let print_allocator a = match a with
    | Spill (vreg, reg) -> Printf.printf "spill %s v%d\n" reg vreg
    | Fill (vreg, reg) -> Printf.printf "fill %s v%d\n" reg vreg
    | Assign (vreg, reg) -> Printf.printf "assign v%d %s\n" vreg reg
    | Instr instr -> print_endline (String.concat "" (List.map string_of_arm_part instr))

(* ARM register assignments:
   R0-3   arguments + scratch
   R4     static link or scratch
   R5-10  callee-save temps
   R11=fp frame pointer
   R12=sp stack pointer
   R13=ip temp for linkage
   R14=lr link register
   R15=pc program counter *)
let regs = ["r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"; "fp"; "sp"; "ip"; "lr"; "pc"]
let regs_volatile = ["r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"]
let regs_stable = ["r5"; "r6"; "r7"; "r8"; "r9"; "r10"; "r0"; "r1"; "r2"; "r3"; "r4"]

let arm_rules = [
    Rule (
        "call",
        PNode (PCALL 0, [Term (GLOBAL "")]),
        1,
        fun [Right (PCALL args); Right (GLOBAL f)] -> [ArmInstrNode ([Lit ("bl " ^ f)], [])]
    );
    Rule (
        "call",
        PNode (PCALL 0, [NonTerm "reg"]),
        1,
        fun [Right (PCALL args); Left r] -> [ArmInstrNode ([Lit "blx "; In], [r])]
    );
    Rule (
        "op",
        NonTerm "reg",
        0,
        fun [Left r] -> [ArmInstrPart ([In], [r])]
    );
    Rule (
        "op",
        Term (CONST 0),
        0,
        fun [Right (CONST x)] -> [ArmInstrPart ([Lit ("#" ^ string_of_int x)], [])]
    );
    Rule (
        "addr",
        Term (LOCAL 0),
        0,
        fun [Right (LOCAL x)] -> [ArmInstrPart ([Lit ("[fp, #" ^ string_of_int x ^ "]")], [])]
    );
    Rule (
        "addr",
        PNode (BINOP PlusA, [NonTerm "reg"; Term (CONST 0)]),
        0,
        fun [_; Left r; Right (CONST x)] -> [ArmInstrPart ([Lit "["; In; Lit (", #" ^ string_of_int x); Lit "]"], [r])]
    );
    Rule (
        "addr",
        PNode (BINOP PlusA, [NonTerm "reg"; NonTerm "reg"]),
        0,
        fun [_; Left r0; Left r1] -> [ArmInstrPart ([Lit "["; In; Lit ", "; In; Lit "]"], [r0; r1])]
    );
    Rule (
        "addr",
        NonTerm "reg",
        0,
        fun [Left r] -> [ArmInstrPart ([Lit "["; In; Lit "]"], [r])]
    );
    Rule (
        "reg",
        NonTerm "call",
        0,
        fun [Left r] -> [r]
    );
    Rule (
        "reg",
        Term (GLOBAL ""),
        1,
        fun [Right (GLOBAL x)] -> [ArmInstrNode ([Lit "ldr "; Out; Lit (", =_" ^ x)], [])]
    );
    Rule (
        "reg",
        Term (CONST 0),
        1,
        fun [Right (CONST x)] -> [ArmInstrNode ([Lit "mov "; Out; Lit (", #" ^ string_of_int x)], [])]
    );
    Rule (
        "reg",
        Term (LOCAL 0),
        1,
        fun [Right (LOCAL x)] -> [ArmInstrNode ([Lit "add "; Out; Lit (", fp, #" ^ string_of_int x)], [])]
    );
    Rule (
        "reg",
        Term (TEMP 0),
        0,
        fun [Right (TEMP x)] -> [ArmUseTemp x]
    );
    Rule (
        "reg",
        PNode (LOADW, [NonTerm "addr"]),
        1,
        fun [_; Left a] -> [ArmInstrNode ([Lit "ldr "; Out; Lit ", "; In], [a])]
    );
    Rule (
        "reg",
        PNode (BINOP Minus, [NonTerm "reg"; NonTerm "op"]),
        1,
        fun [_; Left r; Left o] -> [ArmInstrNode ([Lit "sub "; Out; Lit ", "; In; Lit ", "; In], [r; o])]
    );
    Rule (
        "reg",
        PNode (DEFTMP 0, [NonTerm "reg"]),
        0,
        fun [Right (DEFTMP x); Left r] -> [ArmDefTemp (x, r)]
    );
    Rule (
        "reg",
        PNode (BINOP PlusA, [NonTerm "reg"; NonTerm "op"]),
        1,
        fun [_; Left r; Left o] -> [ArmInstrNode ([Lit "add "; Out; Lit ", "; In; Lit ", "; In], [r; o])]
    );
    Rule (
        "reg",
        PNode (BINOP Lsl, [NonTerm "reg"; NonTerm "op"]),
        1,
        fun [_; Left r; Left o] -> [ArmInstrNode ([Lit "lsl "; Out; Lit ", "; In; Lit ", "; In], [r; o])]
    );
    Rule (
        "reg",
        PNode (BINOP Times, [NonTerm "reg"; NonTerm "op"]),
        1,
        fun [_; Left r; Left o] -> [ArmInstrNode ([Lit "mul "; Out; Lit ", "; In; Lit ", "; In], [r; o])]
    );
    Rule (
        "reg",
        PNode (BINOP Plus, [NonTerm "reg"; NonTerm "op"]),
        1,
        fun [_; Left r; Left o] -> [ArmInstrNode ([Lit "add "; Out; Lit ", "; In; Lit ", "; In], [r; o])]
    );
    Rule (
        "stmt",
        PNode (STOREW, [NonTerm "reg"; NonTerm "addr"]),
        1,
        fun [_; Left r; Left a] -> [ArmInstrNode ([Lit "str "; In; Lit ", "; In], [r; a])]
    );
    Rule (
        "stmt",
        NonTerm "call",
        0,
        fun [Left r] -> [r]
    );
    Rule (
        "stmt",
        NonTerm "reg",
        0, 
        fun [Left r] -> [r]
    );
    Rule (
        "stmt",
        Term (JUMP 0),
        0,
        fun [Right (JUMP l)] -> [ArmInstrNode ([Lit ("b .L" ^ string_of_int l)], [])]
    );
    Rule (
        "stmt",
        GPNode (ARG 0, (fun (ARG n) -> n < 4), [NonTerm "reg"]),
        1,
        fun [Right (ARG n); Left r] -> [ArmInstrNode ([Lit ("mov r" ^ string_of_int n ^ ", "); In], [r])]
    );
    Rule (
        "stmt",
        GPNode (ARG 0, (fun (ARG n) -> n >= 4), [NonTerm "reg"]),
        1,
        fun [Right (ARG n); Left r] -> [ArmInstrNode ([Lit "str "; In; Lit (", [sp" ^ string_of_int (4 * n - 16) ^ "]")], [r])]
    );
    Rule (
        "stmt",
        PNode (JUMPC (Neq, 0), [NonTerm "reg"; NonTerm "op"]),
        0,
        fun [Right (JUMPC (_, l)); Left r; Left o] -> [ArmInstrNode ([Lit "cmp "; In; Lit ", "; In], [r; o]); ArmInstrNode ([Lit ("bne .L" ^ string_of_int l)], [])]
    );
    Rule (
        "stmt",
        PNode (JUMPC (Eq, 0), [NonTerm "reg"; NonTerm "op"]),
        0,
        fun [Right (JUMPC (_, l)); Left r; Left o] -> [ArmInstrNode ([Lit "cmp "; In; Lit ", "; In], [r; o]); ArmInstrNode ([Lit ("beq .L" ^ string_of_int l)], [])]
    );
    Rule (
        "stmt",
        PNode (JUMPC (Lt, 0), [NonTerm "reg"; NonTerm "op"]),
        0,
        fun [Right (JUMPC (_, l)); Left r; Left o] -> [ArmInstrNode ([Lit "cmp "; In; Lit ", "; In], [r; o]); ArmInstrNode ([Lit ("blt .L" ^ string_of_int l)], [])]
    );
    Rule (
        "stmt",
        PNode (JUMPC (Gt, 0), [NonTerm "reg"; NonTerm "op"]),
        0,
        fun [Right (JUMPC (_, l)); Left r; Left o] -> [ArmInstrNode ([Lit "cmp "; In; Lit ", "; In], [r; o]); ArmInstrNode ([Lit ("bgt .L" ^ string_of_int l)], [])]
    );
    Rule (
        "stmt",
        Term (LABEL 0),
        0,
        fun [Right (LABEL l)] -> [ArmInstrNode ([Lit (".L" ^ string_of_int l ^ ":")], [])]
    );
    Rule (
        "stmt",
        PNode (LABEL 0, [NonTerm "stmt"]),
        0,
        fun [Right (LABEL l); Left r] -> [ArmInstrNode ([Lit (".L" ^ string_of_int l ^ ":")], [r])]
    );
    Rule (
        "stmt",
        PNode (SLINK, [GTerm (CONST 0, (fun (CONST k) -> k = 0))]),
        0,
        fun [_; _] -> []
    );
    Rule (
        "stmt",
        PNode (SLINK, [NonTerm "reg"]),
        1,
        fun [_; Left r] -> [ArmInstrNode ([Lit "mov r4, "; In], [r])]
    );
    Rule (
        "stmt",
        PNode (LINE 0, [NonTerm "stmt"]),
        0,
        fun [Right (LINE n); Left r] -> [ArmInstrNode ([Lit ("@Line " ^ string_of_int n)], [r])]
    );
    Rule (
        "stmt",
        PNode (RESULTW, [NonTerm "reg"]),
        1,
        fun [_; Left r] -> [ArmInstrNode ([Lit "mov r0, "; In], [r])]
    );
    Rule (
        "stmt",
        Term (LINE 0),
        0,
        fun [Right (LINE n)] -> [ArmInstrNode ([Lit ("@Line " ^ string_of_int n)], [])]
    );
]

let rec eliminate_parts tree = match tree with
      ArmInstrNode ([], []) -> tree
    | ArmInstrNode (p :: ps, cs) -> (match p with
          Lit l -> let ArmInstrNode (ps', cs') = eliminate_parts (ArmInstrNode (ps, cs))
                   in ArmInstrNode (Lit l :: ps', cs')
        | In -> let cs_hd = List.hd cs and cs_tl = List.tl cs in (match cs_hd with
              ArmInstrPart (pp, pc) -> eliminate_parts (ArmInstrNode (pp @ ps, pc @ cs_tl))
            | _ -> let ArmInstrNode (ps', cs') = eliminate_parts (ArmInstrNode (ps, cs_tl))
                   in ArmInstrNode (In :: ps', (eliminate_parts cs_hd) :: cs'))
        | Out -> let ArmInstrNode (ps', cs') = eliminate_parts (ArmInstrNode (ps, cs))
                 in ArmInstrNode (Out :: ps', cs')
        | _-> failwith "Unable to form complete instruction."
    )
    | ArmDefTemp (temp_id, code) -> ArmDefTemp (temp_id, eliminate_parts code)
    | ArmUseTemp temp_id -> tree
    | _ -> failwith "Unable to form complete instruction."
    
let rec dag_of_arm_instr_tree tree = match tree with
    | ArmInstrNode (parts, children) -> DagNode (parts, List.map dag_of_arm_instr_tree children)
    | ArmDefTemp (label, tree) -> DagRoot (label, dag_of_arm_instr_tree tree)
    | ArmUseTemp label -> DagEdge label
    | ArmInstrPart _ -> failwith "Unable to transform incomplete instruction to DAG node."

let rec get_vreg_id node = match node with
    | DagEdge reg -> reg
    | DagNode ((ps, reg), cs) -> reg
    | DagRoot (label, child) -> get_vreg_id child

let get_vreg node = VReg (get_vreg_id node)

let rec vreg_tree_of_vreg_dag dag = match dag with
    | DagEdge reg -> DagEdge reg
    | DagNode (([], reg), []) -> DagNode (([], reg), [])
    | DagNode ((In :: ps, reg), c :: cs) -> combine (get_vreg c) (Some (vreg_tree_of_vreg_dag c)) (vreg_tree_of_vreg_dag (DagNode ((ps, reg), cs)))
    | DagNode ((Out :: ps, reg), cs) -> combine (VReg reg) None (vreg_tree_of_vreg_dag (DagNode ((ps, reg), cs)))
    | DagNode ((Lit l :: ps, reg), cs) -> combine (Lit l) None (vreg_tree_of_vreg_dag (DagNode ((ps, reg), cs)))
    | DagNode (([], reg), [child]) -> (
        match vreg_tree_of_vreg_dag child with 
            | DagNode ((ps, reg_child), cs) -> DagNode ((ps, reg), cs)
            | _ -> failwith "Failed to reduce virtual register DAG to tree."
    )
    | DagRoot (label, child) -> DagRoot (label, vreg_tree_of_vreg_dag child)
    | _ -> failwith "Failed to reduce virtual register DAG to tree."
and combine part child node = match node with 
    | DagNode ((parts, reg), cs) -> 
        (match child with
            | None -> DagNode ((part :: parts, reg), cs)
            | Some c -> DagNode ((part :: parts, reg), c :: cs)
        )
    | _ -> failwith "Failed to reduce virtual register DAG to tree."
    
let regs_of_instr set instr = match instr with
    | Lit l :: _ ->
        if starts_with l "call" then ["r0"]
        else set
    | _ -> failwith "Unable to recognize instruction."

let trashed_regs instr = match instr with
    | Lit l :: _ ->
        if starts_with l "call" then ["r0"; "r1"; "r2"; "r3"]
        else if starts_with l "mov r0" then ["r0"]
        else if starts_with l "mov r1" then ["r1"]
        else if starts_with l "mov r2" then ["r2"]
        else if starts_with l "mov r3" then ["r3"]
        else if starts_with l "mov r4" then ["r4"]
        else []
    | _ -> failwith "Unable to recognize instruction."
    
let rec vreg_list_of_vreg_tree tree = match tree with
    | DagNode ((parts, reg), children) -> (parts, reg, List.map get_vreg_id children, regs_of_instr regs_volatile parts, trashed_regs parts) :: List.concat (List.map vreg_list_of_vreg_tree children)
    | DagEdge _ -> []
    | DagRoot (label, child) -> (
        match vreg_list_of_vreg_tree child with
            | [] -> failwith "Unable to convert tree of instructions to list of instructions."
            | (parts, out_reg, in_regs, _, trashed_regs) :: is -> (parts, out_reg, in_regs, regs_of_instr regs_stable parts, trashed_regs) :: is)
    
let translate irs =
    let arm_dags = List.concat (List.map (fun ir -> List.map dag_of_arm_instr_tree (List.map eliminate_parts (translate "stmt" arm_rules ir))) irs) in
    let vreg_dags = vreg_alloc arm_dags in
    let vreg_trees = List.map vreg_tree_of_vreg_dag vreg_dags in
    let vreg_lists = List.map List.rev (List.map vreg_list_of_vreg_tree vreg_trees) in
    let reg_alloc_ops = reg_alloc regs_stable (List.concat vreg_lists) in
    reg_alloc_ops
    
let rec count_spills allocs = match allocs with
    | [] -> 0
    | (Spill _) :: allocs -> 1 + count_spills allocs
    | _ :: allocs -> count_spills allocs
    
let process_allocs instrs =
    let map = Hashtbl.create 1 in
    let code = ref [] in
    let reg_of_vreg part = match part with
        | VReg vreg -> PReg (Hashtbl.find map vreg)
        | part -> part
    in
    let gen instr = match instr with
        | Assign (vreg, reg) ->
            Hashtbl.replace map vreg reg;
            code := [Lit ("@Assign " ^ string_of_int vreg ^ " with " ^ reg)] :: !code
        | Instr parts ->
            code := (List.map reg_of_vreg parts) :: !code
        | Move (vreg, reg) ->
            let current_reg = Hashtbl.find map vreg in
            Hashtbl.replace map vreg reg;
            code := [Lit ("@Move " ^ string_of_int vreg ^ " with " ^ reg)] :: [Lit ("mov " ^ reg ^ ", " ^ current_reg)] :: !code
        | Spill (vreg, reg) -> code := [Lit ("@Spill " ^ string_of_int vreg ^ " with " ^ reg)] :: !code
        | Fill (vreg, reg) -> code := [Lit ("@Fill " ^ string_of_int vreg ^ " with " ^ reg)] :: !code
    in
    List.iter gen instrs;
    List.rev !code
    
let translate_proc (label, proc_level, num_args, num_reg_vars, frame_size, irs) = 
    let alloc_body = translate irs in
    let spills = count_spills alloc_body in
    let spills_size = 4 * spills in
    let bodies = process_allocs alloc_body in
    [Lit (label ^ ":")] :: bodies
    
let translate_global (label, size) = [[Lit (".comm " ^ label ^ ", " ^ string_of_int size ^ ", 4")]]
    
let translate_string (label, text) =
    let bytes = ref [] in
    String.iter (fun c -> bytes := (int_of_char c) :: !bytes) text;
    [[Lit (label ^ ": ")]; [Lit (".byte " ^ String.concat ", " (List.map string_of_int (List.rev !bytes))); Lit ", 0"]]
    
let translate_progr globals procs strings =
    let globals_asm = if globals = [] then [] else [Lit ".data"] :: List.concat (List.map translate_global globals) in
    let procs_asm = [Lit ".text"] :: List.concat (List.map translate_proc procs) in
    let strings_asm = if strings = [] then [] else [Lit ".data"] :: List.concat (List.map translate_string strings) in
    let preamble = [[Lit ".global pmain"]] in
    let postamble = [] in
    preamble @ procs_asm @ globals_asm @ strings_asm @ postamble

let string_of_instr instr =
    let instr_str = String.concat "" (List.map string_of_arm_part instr) in
    if not (starts_with instr_str ".") && not (ends_with instr_str ":") then "    " ^ instr_str
    else if ends_with instr_str ":" && starts_with instr_str "." then "  " ^ instr_str
    else instr_str
