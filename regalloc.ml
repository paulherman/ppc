open Util

type 'a vreg_dag = (int, 'a * int, int) Util.dag

type 'a asm_dag = (int, 'a, int) Util.dag

type ('a, 'b) reg_dag = (int, 'a * 'b, 'b) Util.dag

type 'a linear_instrs = ('a * int * int list) list

type 'a allocator =
    | Spill of 'a * int * int * int
    | Fill of 'a * int * int * int
    | Assign of int * 'a * int

let get_vreg vreg_dag = match vreg_dag with
    | DagNode ((code, vreg), children) -> vreg
    | DagEdge vreg -> vreg
    | DagRoot _ -> failwith "Unable to get register from DAG."

(* Augment instruction DAG with virtual registers from an infinite pool. *)
let rec vreg_alloc trees = 
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
        if Hashtbl.mem mapping label then (regs, DagEdge (Hashtbl.find mapping label)) 
        else failwith ("Unable to find register " ^ string_of_int label ^ " for label from DAG.")
    
(* Compute liveness intervals of virtual registers. *)
let intervals_of_instrs instrs = 
    let rec set_interval_of_instr starts ends count (instr, out_reg, in_regs) =
        Hashtbl.replace starts out_reg (count + 0);
        List.iter (fun reg -> Hashtbl.replace ends reg count) in_regs in
    let reg_interval intervals ends vreg start = if Hashtbl.mem ends vreg then intervals := (vreg, start, Hashtbl.find ends vreg) :: !intervals in
    let interval_compare (reg0, start0, end0) (reg1, start1, end1) = if start0 == start1 then 0 else if start0 > start1 then 1 else -1 in
    let intervals = ref [] in
    let starts_map = Hashtbl.create 1 in
    let ends_map = Hashtbl.create 1 in
    List.iteri (set_interval_of_instr starts_map ends_map) instrs;
    Hashtbl.iter (reg_interval intervals ends_map) starts_map;
    let sorted_intervals = List.sort interval_compare !intervals in
    List.iter (fun (vreg, istart, iend) -> Printf.printf "interval v%d: %d -> %d\n" vreg istart iend) sorted_intervals;
    sorted_intervals
    
(* Choose interval to spill based on the end point. *)
let choose_spill_interval intervals regs_needed =
    let interval_compare (vreg0, reg0, start0, end0) (vreg1, reg1, start1, end1) = 0 - (compare end0 end1) in
    let end_sorted_intervals = List.sort interval_compare !intervals in
    let rec pick is = match is with
        | [] -> failwith "Unable to find a virtual register to spill."
        | (vreg, reg, istart, iend) :: is -> 
            if List.exists (fun r -> r == vreg) regs_needed then 
                let (remove, is') = pick is in (remove, (vreg, reg, istart, iend) :: is')
            else ((vreg, reg, istart, iend), is)
    in
    let (remove, remaining) = pick end_sorted_intervals in
    intervals := remaining;
    remove
    
let intervals_of_vregs intervals =
    let map = Hashtbl.create 1 in
    List.iter (fun (vreg, istart, iend) -> Hashtbl.add map vreg (vreg, istart, iend)) intervals;
    map
    
let spill intervals spills operations spill_id count regs_needed =
    let (spilled_vreg, spilled_reg, spilled_start, spilled_end) = choose_spill_interval intervals regs_needed in
    operations := Spill (spilled_reg, spilled_vreg, !spill_id, count) :: !operations;
    Hashtbl.add spills spilled_vreg !spill_id;
    spill_id := !spill_id + 1;
    spilled_reg
    
let rec close_intervals intervals used_regs count = match intervals with
    | [] -> []
    | (vreg, reg, istart, iend) :: is ->
        let is' = close_intervals is used_regs count in
        if iend < count then (used_regs := List.filter (fun r -> r != reg) !used_regs; is')
        else (vreg, reg, istart, iend) :: is'
        
let print_regs out_reg in_regs = print_endline (String.concat ", " ((string_of_int out_reg) :: (List.map string_of_int in_regs)))
    
(* TODO:
    - spill registers that become dirty after one instruction
    - allow in registers to be specified
    - remove lots of duplication
*)
let rec reg_alloc regs instrs regs_of_instr = 
    let active_intervals = ref [] in
    let current_instrs = ref instrs in
    let used_regs = ref [] in
    let count = ref 0 in
    let intervals = ref (intervals_of_instrs instrs) in
    let operations = ref [] in
    let spill_id = ref 0 in
    let interval_of_vreg = intervals_of_vregs !intervals in
    let spills = Hashtbl.create 1 in
    while !current_instrs != [] do
        let (instr, out_reg, in_regs) = List.hd !current_instrs in
        let regs_needed = out_reg :: in_regs in
        active_intervals := close_intervals !active_intervals used_regs !count;
        current_instrs := List.tl !current_instrs;
        if !intervals != [] then begin
            let (vreg, istart, iend) = List.hd !intervals in
            if istart = !count then begin
                let possible_regs = list_diff (regs_of_instr instr) !used_regs in
                let allocated_reg = 
                    if possible_regs = [] then spill active_intervals spills operations spill_id !count regs_needed
                    else List.hd possible_regs in
                operations := Assign (vreg, allocated_reg, !count) :: !operations;
                used_regs := allocated_reg :: !used_regs;
                intervals := List.tl !intervals;
                active_intervals := (vreg, allocated_reg, istart, iend) :: !active_intervals
            end;
            let filled_intervals = ref [] in
            List.iter (fun vreg -> 
                if Hashtbl.mem spills vreg then begin
                    let spilled_id = Hashtbl.find spills vreg in
                    let possible_regs = list_diff regs !used_regs in
                    let allocated_reg = 
                        if possible_regs = [] then spill active_intervals spills operations spill_id !count regs_needed
                        else List.hd possible_regs in
                    operations := Fill (allocated_reg, vreg, spilled_id, !count) :: !operations;
                    filled_intervals := (vreg, allocated_reg) :: !filled_intervals
                end
            ) in_regs;
            List.iter (fun (vreg, reg) ->
                let (vreg, istart, iend) = Hashtbl.find interval_of_vreg vreg in
                active_intervals := (vreg, reg, istart, iend) :: !active_intervals
            ) !filled_intervals
        end;
        count := 1 + !count
    done;
    List.rev !operations

