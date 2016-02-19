type arm_instr

type global = Keiko.symbol * int

type proc = Keiko.symbol * int * int * int * int * Keiko.optree list

type str_literal = Keiko.symbol * string

val translate_progr : global list -> proc list -> str_literal list -> arm_instr list

val string_of_instr : arm_instr -> string
