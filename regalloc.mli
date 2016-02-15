type 'a vreg_dag = (int, 'a * int, int) Util.dag

type 'a asm_dag = (int, 'a, int) Util.dag

type ('a, 'b) reg_dag = (int, 'a * 'b, 'b) Util.dag

val vreg_alloc : 'a asm_dag -> 'a vreg_dag

val vreg_alloc_many : 'a asm_dag list -> 'a vreg_dag list

val reg_alloc : 'a vreg_dag -> 'b list -> ('a -> 'b list) -> ('b -> (('a, 'b) reg_dag * int)) -> (('b * int) -> ('a, 'b) reg_dag) -> ('a, 'b) reg_dag
