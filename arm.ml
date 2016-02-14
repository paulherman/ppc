open Gen
open Keiko

type arm_part =
      Lit of string
    | Out
    | In
    | Reg of int
    
type arm_instr_tree =
      ArmInstrNode of arm_part list * arm_instr_tree list
    | ArmInstrPart of arm_part list * arm_instr_tree list
    | ArmDefTemp of int * arm_instr_tree
    | ArmUseTemp of int

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
                 in ArmInstrNode (Out :: ps', cs'))
    | ArmDefTemp (temp_id, code) -> ArmDefTemp (temp_id, eliminate_parts code)
    | ArmUseTemp temp_id -> tree

let rec flatten_arm_instr_tree tree = match tree with
      ArmInstrNode (parts, children) -> List.concat (List.map flatten_arm_instr_tree children) @ [parts]
    | ArmDefTemp (temp_id, temp_tree) -> flatten_arm_instr_tree temp_tree
    | _ -> []

let string_of_arm_part part = match part with
      Lit lit -> lit
    | In -> "$IN"
    | Out -> "$OUT"
    | Reg reg_num -> String.concat "" ["r"; string_of_int reg_num]

let rec string_of_arm_instr instr = String.concat "" (List.map string_of_arm_part instr)

let rec string_of_arm_instrs instrs = match instrs with
      [] -> ""
    | i :: is -> String.concat "\n" [string_of_arm_instr i; string_of_arm_instrs is]

let arm_translate ir = eliminate_parts (translate "stmt" arm_rules ir)

let string_of_arm_instr_tree tree = string_of_arm_instrs (flatten_arm_instr_tree tree)
