type arm_part =
    | Lit of string
    | Out
    | In
    | VReg of int
    | PReg of string

val arm_translate : Keiko.optree -> (arm_part list * int * int list) list
