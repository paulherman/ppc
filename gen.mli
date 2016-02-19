type pattern =
    | NonTerm of string
    | Term of Keiko.inst
    | GTerm of Keiko.inst * (Keiko.inst -> bool)
    | GPNode of Keiko.inst * (Keiko.inst -> bool) * pattern list
    | PNode of Keiko.inst * pattern list
             
type ('a, 'b) either = Left of 'a | Right of 'b

type 'a instr_gen = ('a, Keiko.inst) either list -> 'a list

type 'a rule = Rule of string * pattern * int * 'a instr_gen

val translate : string -> 'a rule list -> Keiko.optree -> 'a list
