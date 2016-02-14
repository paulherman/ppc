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

val arm_rules : arm_instr_tree Gen.rule list

val arm_translate : Keiko.optree -> arm_instr_tree

val string_of_arm_instr_tree : arm_instr_tree -> string
