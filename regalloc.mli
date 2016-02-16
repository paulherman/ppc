type 'a vreg_dag = (int, 'a * int, int) Util.dag

type 'a asm_dag = (int, 'a, int) Util.dag

type ('a, 'b) reg_dag = (int, 'a * 'b, 'b) Util.dag

type 'a linear_instrs = ('a * int * int list) list

val vreg_alloc : 'a asm_dag -> 'a vreg_dag

val vreg_alloc_many : 'a asm_dag list -> 'a vreg_dag list

val reg_alloc : 'a linear_instrs  -> 'b list -> ('a -> 'b list) -> ('a -> 'b list) -> ('b -> 'a linear_instrs) -> (('b * int) ->  'a linear_instrs) -> (int, 'b) Hashtbl.t

val get_vreg : 'a vreg_dag -> int
