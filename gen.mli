type pattern = NonTerm of string
             | Term of Keiko.inst
             | PNode of Keiko.inst * pattern list
             
type ('a, 'b) either = Left of 'a | Right of 'b

type 'a instr_gen = ('a, Keiko.inst) either list -> 'a

type 'a rule = Rule of string * pattern * int * 'a instr_gen

val translate : string -> 'a rule list -> Keiko.optree -> 'a
