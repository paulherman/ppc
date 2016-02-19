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

let string_of_arm_part part = match part with
    | Lit lit -> lit
    | In -> "$IN"
    | Out -> "$OUT"
    | VReg reg_num -> "v" ^ string_of_int reg_num
    | PReg reg -> reg
    
let print_instr i = match i with
    | (instr, out_reg, in_regs) -> print_endline (String.concat "" (List.map string_of_arm_part instr))
 
  
let print_allocator a = match a with
    | Spill (reg, vreg) -> Printf.printf "spill %s v%d\n" reg vreg
    | Fill (reg, vreg) -> Printf.printf "fill %s v%d\n" reg vreg
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

let free_regs = ["r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"]

let arm_rules = [
    Rule ("addr", Term (LOCAL 0), 0, fun args -> match args with [Right (LOCAL x)]-> ArmInstrPart ([Lit ("[fp, #" ^ string_of_int x ^ "]")], []));
    Rule ("reg", Term (CONST 0), 1, fun args -> match args with [Right (CONST x)]-> ArmInstrNode ([Lit "mov "; Out; Lit (", #" ^ string_of_int x)], []));
    Rule ("op", Term (CONST 0), 0, fun args -> match args with [Right (CONST x)]-> ArmInstrPart ([Lit ("#" ^ string_of_int x)], []));
    Rule ("reg", Term (GLOBAL ""), 1, fun args -> match args with [Right (GLOBAL x)]-> ArmInstrNode ([Lit "ldr "; Out; Lit (", =_" ^ x)], []));
    Rule ("reg", Term (LOCAL 0), 1, fun args -> match args with [Right (LOCAL x)]-> ArmInstrNode ([Lit "add "; Out; Lit (", fp, #" ^ string_of_int x)], []));
    Rule ("reg", Term (TEMP 0), 0, fun args -> match args with [Right (TEMP x)] -> ArmUseTemp x);
    Rule ("reg", PNode (LOADW, [NonTerm "addr"]), 1, fun args -> match args with [_; Left a]-> ArmInstrNode ([Lit "ldr "; Out; Lit ", "; In], [a]));
    Rule ("reg", PNode (BINOP Minus, [NonTerm "reg"; NonTerm "op"]), 1, fun args -> match args with [_; Left r; Left o] -> ArmInstrNode ([Lit "sub "; Out; Lit ", "; In; Lit ", "; In], [r; o]));
    Rule ("reg", PNode (DEFTMP 0, [NonTerm "reg"]), 0, fun args -> match args with [Right (DEFTMP x); Left r]-> ArmDefTemp (x, r));
    Rule ("stmt", PNode (STOREW, [NonTerm "reg"; NonTerm "addr"]), 1, fun args -> match args with [_; Left r; Left a] -> ArmInstrNode ([Lit "str "; In; Lit ", "; In], [r; a]));
    Rule ("addr", PNode (BINOP PlusA, [NonTerm "reg"; Term (CONST 0)]), 0, fun args -> match args with [_; Left r; Right (CONST x)]-> ArmInstrPart ([Lit "["; In; Lit (", #" ^ string_of_int x)], [r]));
    Rule ("addr", PNode (BINOP PlusA, [NonTerm "reg"; NonTerm "reg"]), 0, fun args -> match args with [_; Left r0; Left r1] -> ArmInstrPart ([Lit "["; In; Lit ", "; In; Lit "]"], [r0; r1]));
    Rule ("reg", PNode (BINOP PlusA, [NonTerm "reg"; NonTerm "op"]), 1, fun args -> match args with [_; Left r; Left o] -> ArmInstrNode ([Lit "add "; Out; Lit ", "; In; Lit ", "; In], [r; o]));
    Rule ("reg", PNode (BINOP Lsl, [NonTerm "reg"; NonTerm "op"]), 1, fun args -> match args with [_; Left r; Left o] -> ArmInstrNode ([Lit "lsl "; Out; Lit ", "; In; Lit ", "; In], [r; o]));
    Rule ("reg", PNode (BINOP Times, [NonTerm "reg"; NonTerm "op"]), 1, fun args -> match args with [_; Left r; Left o] -> ArmInstrNode ([Lit "mul "; Out; Lit ", "; In; Lit ", "; In], [r; o]));
    Rule ("reg", PNode (BINOP Plus, [NonTerm "reg"; NonTerm "op"]), 1, fun args -> match args with [_; Left r; Left o] -> ArmInstrNode ([Lit "add "; Out; Lit ", "; In; Lit ", "; In], [r; o]));
    Rule ("op", NonTerm "reg", 0, fun args -> match args with [Left r] -> ArmInstrPart ([In], [r]));
    Rule ("addr", NonTerm "reg", 0, fun args -> match args with [Left r] -> ArmInstrPart ([Lit "["; In; Lit "]"], [r]));
    Rule ("stmt", NonTerm "reg", 0, fun args -> match args with [Left r] -> r);
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
    | _ -> failwith "Failed to reduce virtual register DAG to tree."
and combine part child node = match node with 
    | DagNode ((parts, reg), cs) -> 
        (match child with
            | None -> DagNode ((part :: parts, reg), cs)
            | Some c -> DagNode ((part :: parts, reg), c :: cs)
        )
    | _ -> failwith "Failed to reduce virtual register DAG to tree."
    
let rec vreg_list_of_vreg_tree tree = match tree with
    | DagNode ((parts, reg), children) -> List.concat (List.map vreg_list_of_vreg_tree children) @ [(parts, reg, List.map get_vreg_id children)]
    | DagEdge _ -> []
    | DagRoot (label, child) -> vreg_list_of_vreg_tree child
    
let regs_of_instr instr = match instr with
    | Lit l :: _ ->
        if starts_with l "call" then ["r0"]
        else free_regs
    | _ -> failwith "Unable to recognize instruction."

let translate irs =
    let arm_dags = List.map (fun ir -> dag_of_arm_instr_tree (eliminate_parts (translate "stmt" arm_rules ir))) irs in
    let vreg_dags = vreg_alloc arm_dags in
    let vreg_trees = List.map vreg_tree_of_vreg_dag vreg_dags in
    let vreg_lists = List.map vreg_list_of_vreg_tree vreg_trees in
    let reg_alloc_ops = reg_alloc free_regs regs_of_instr (List.concat vreg_lists) in
    List.iter print_allocator reg_alloc_ops;
    List.concat vreg_lists
    
let translate_proc (label, level, nargs, nregv, fsize, ir) = translate [ir]
    
let translate_progr globals procs strings = []

let string_of_instr instr = String.concat "" (List.map string_of_arm_part instr)
