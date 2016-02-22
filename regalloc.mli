type 'a vreg_dag = (int, 'a * int, int) Util.dag

type 'a asm_dag = (int, 'a, int) Util.dag

type ('a, 'b) reg_dag = (int, 'a * 'b, 'b) Util.dag

type ('a, 'b) vreg_instr = 'a * int * (int list) * ('b list) * ('b list)

type ('a, 'b) allocator =
    | Spill of 'b * int
    | Fill of 'b * int
    | Assign of int * 'b
    | Move of int * 'b
    | Instr of 'a

val vreg_alloc : 'a asm_dag list -> 'a vreg_dag list

val reg_alloc : 'b list -> (('a, 'b) vreg_instr) list -> ('a, 'b) allocator list

val get_vreg : 'a vreg_dag -> int
