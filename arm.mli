type arm_part =
      Lit of string
    | Out
    | In
    | Reg of int

val arm_translate : Keiko.optree -> arm_part list Regalloc.vreg_dag
