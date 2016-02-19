type arm_part =
    | Lit of string
    | Out
    | In
    | VReg of int
    | PReg of string
    
type arm_instr = arm_part list

val translate : Keiko.optree list -> (arm_instr, string) Regalloc.allocator list

val translate_progr : (Keiko.symbol * int) list -> (Keiko.symbol * int * int * int * int * Keiko.optree list) list -> (Keiko.symbol * string) list -> arm_instr list

val string_of_instr : arm_instr -> string
