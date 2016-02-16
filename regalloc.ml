open Util

type 'a vreg_dag = (int, 'a * int, int) Util.dag

type 'a asm_dag = (int, 'a, int) Util.dag

type ('a, 'b) reg_dag = (int, 'a * 'b, 'b) Util.dag

type 'a linear_instrs = ('a * int * int list) list

let get_vreg vreg_dag = match vreg_dag with
    | DagNode ((code, vreg), children) -> vreg
    | DagEdge vreg -> vreg
    | DagRoot _ -> failwith "Unable to get register from DAG."

let rec vreg_alloc_many trees = 
    let mapping = Hashtbl.create 1 in
    let (_, trees') = vreg_alloc_many' 0 mapping trees in
    trees'
and vreg_alloc_many' regs mapping trees = match trees with
    | [] -> (regs, [])
    | t :: ts ->
        let (regs', t') = vreg_alloc_one regs mapping t in
        let (regs'', ts') = vreg_alloc_many' regs' mapping ts in
        (regs'', t' :: ts')
and vreg_alloc_one regs mapping node = match node with
    | DagRoot (label, child) ->
        let (regs', child') = vreg_alloc_one regs mapping child in
        Hashtbl.add mapping label (get_vreg child');
        (regs', child')
    | DagNode (value, children) ->
        let (regs', children') = vreg_alloc_many' regs mapping children in
        (1 + regs', DagNode ((value, regs'), children'))
    | DagEdge label -> 
        if Hashtbl.mem mapping label then (regs, DagEdge (Hashtbl.find mapping label)) else failwith "Unable to find register for label from DAG."
    
let vreg_alloc tree = match vreg_alloc_many [tree] with
    | [tree'] -> tree'
    | _ -> failwith "Unable to allocate single tree."
    
let reg_alloc instrs regs get_instr_regs get_instr_dirty_regs spill fill = 
    let rec set_interval_of_instr intervals count (instr, out_reg, in_regs) =
        Hashtbl.add intervals out_reg (count + 1, count + 1);
        List.iter (fun reg -> 
            let (reg_start, reg_end) = Hashtbl.find intervals reg in
            Hashtbl.add intervals reg (reg_start, count)) in_regs
        in
    let reg_interval intervals reg (reg_start, reg_end) = intervals := (reg, reg_start, reg_end) :: !intervals in
    let interval_compare (reg0, start0, end0) (reg1, start1, end1) = compare (start0, end0) (start1, end1) in
    let intervals = ref [] in
    let intervals_map = Hashtbl.create 1 in
    let mapping = Hashtbl.create 1 in
    let active_intervals = ref [] in
    let used_regs = ref [] in
    let current_instrs = ref instrs in
    let count = ref 0 in
    List.iteri (set_interval_of_instr intervals_map) instrs;
    Hashtbl.iter (reg_interval intervals) intervals_map;
    intervals := List.sort interval_compare !intervals;
    while !count < List.length !current_instrs do
        ()
    done;
    mapping
