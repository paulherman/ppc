type 'a vreg_dag = (int, 'a * int, int) Util.dag

type 'a asm_dag = (int, 'a, int) Util.dag

type ('a, 'b) reg_dag = (int, 'a * 'b, 'b) Util.dag

type 'a linear_instrs = ('a * int * int list) list

type 'a allocator =
    | Spill of 'a * int * int * int
    | Fill of 'a * int * int * int
    | Assign of int * 'a * int

val vreg_alloc : 'a asm_dag list -> 'a vreg_dag list

val reg_alloc : 'b list -> 'a linear_instrs -> ('a -> 'b list) -> 'b allocator list

val get_vreg : 'a vreg_dag -> int
