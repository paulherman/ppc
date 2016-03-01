open Util

type 'a vreg_dag = (int, 'a * int, int) Util.dag

type 'a asm_dag = (int, 'a, int) Util.dag

type ('a, 'b) reg_dag = (int, 'a * 'b, 'b) Util.dag

type ('a, 'b) vreg_instr = 'a * int * (int list) * ('b list) * ('b list)

type ('a, 'b) allocator =
    | Spill of int * 'b
    | Fill of int * 'b
    | Assign of int * 'b
    | Move of int * 'b
    | Instr of 'a

let get_vreg vreg_dag = match vreg_dag with
    | DagNode ((code, vreg), children) -> vreg
    | DagEdge vreg -> vreg
    | DagRoot _ -> failwith "Unable to get register from DAG."

(* Augment instruction DAG with virtual registers from an infinite pool. 
   TODO:
    - some instructions output more than one register (i.e. div on x86)
*)
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
        (regs', DagRoot (label, child'))
    | DagNode (value, children) ->
        let (regs', children') = vreg_alloc_many' regs mapping children in
        (1 + regs', DagNode ((value, regs'), children'))
    | DagEdge label ->
        if Hashtbl.mem mapping label then (regs, DagEdge (Hashtbl.find mapping label)) 
        else failwith ("Unable to find register " ^ string_of_int label ^ " for label from DAG.")

(* Get live intervals for virtual registers from instructions list *)
let intervals_of_instrs instrs =
    let intervals = Hashtbl.create 1 in
    List.iteri (fun count (instr, out_reg, in_regs, pref_regs, trash_regs) ->
        Hashtbl.replace intervals out_reg (count, count - 1);
        List.iter (fun in_reg -> Hashtbl.replace intervals in_reg (fst (Hashtbl.find intervals in_reg), count)) in_regs
    ) instrs;
    let intervals_list = List.filter (fun (v, s, e) -> e >= s) (Hashtbl.fold (fun vreg (s, e) is -> (vreg, s, e) :: is) intervals []) in
    List.sort (fun (r0, s0, e0) (r1, s1, e1) -> compare s0 s1) intervals_list

let pregs_of_active_intervals intervals = List.map (fun (vreg, preg, s, e) -> preg) intervals

(* Assign the vreg to some preg, possibly by spilling some other register *)
let preg_of_vreg ops pregs unassignable unspillable active_intervals spilled_intervals vreg s e = 
    let update preg =
        active_intervals := (vreg, preg, s, e) :: !active_intervals;
        preg in
    let pregs = list_diff pregs unassignable in
    if pregs != [] then
        update (List.hd pregs)
    else begin (* Spill another register *)
        let spillable = List.filter (fun (v, p, s, e) -> List.mem p pregs && not (List.mem v unspillable)) !active_intervals in
        match List.sort (fun (v0, p0, s0, e0) (v1, p1, s1, e1) -> compare e1 e0) spillable with
            | [] -> failwith "Unable to find a register to spill."
            | (spilled :: unspilled) -> begin
                let (svreg, preg, s, e) = spilled in
                ops := Spill (svreg, preg) :: !ops;
                spilled_intervals := (svreg, s, e) :: !spilled_intervals;
                active_intervals := List.filter (fun (v, p, s, e) -> v != svreg) !active_intervals;
                update preg
            end
    end

(* Allocate registers for one instruction *)   
let reg_alloc_instr regs intervals active_intervals spilled_intervals count instr =
    let (instr, out_reg, in_regs, pref_regs, trash_regs) = instr in
    let ops = ref [] in
    (* Remove closed intervals *)
    active_intervals := List.filter (fun (vreg, preg, s, e) -> e >= count) !active_intervals;
    (* Spill or move trashed registers *)
    List.iter (fun preg ->
        if List.exists (fun (v, p, s, e) -> preg = p) !active_intervals then begin
            let (vreg, preg, s, e) = List.find (fun (v, p, s, e) -> preg = p) !active_intervals in
            if e > count then begin
                active_intervals := List.filter (fun (v, p, s, e) -> preg != p) !active_intervals;
                let unspillable = in_regs in
                let unassignable = trash_regs @ (pregs_of_active_intervals !active_intervals) in
                let preg = preg_of_vreg ops regs unassignable unspillable active_intervals spilled_intervals vreg s e in
                ops := Move (vreg, preg) :: !ops
            end
        end
    ) trash_regs;
    (* Allocate the out register *)
    if !intervals != [] then begin
        let (out_reg, s, e) = List.hd !intervals in
        if s = count then begin
            let unspillable = in_regs in
            let unassignable = pregs_of_active_intervals !active_intervals in
            let preg = preg_of_vreg ops pref_regs unassignable unspillable active_intervals spilled_intervals out_reg s e in
            ops := Assign (out_reg, preg) :: !ops;
            intervals := List.tl !intervals
        end;
    end;
    (* Fill back spilled in registers *)
    List.iter (fun in_reg ->
        if not (List.exists (fun (vreg, preg, s, e) -> in_reg = vreg) !active_intervals) then begin
            let (in_reg, s, e) = List.find (fun (vreg, s, e) -> vreg = in_reg) !spilled_intervals in
            let unspillable = out_reg :: in_regs in
            let unassignable = trash_regs @ (pregs_of_active_intervals !active_intervals) in
            let preg = preg_of_vreg ops regs unassignable unspillable active_intervals spilled_intervals in_reg s e in
            spilled_intervals := List.filter (fun (v, s, e) -> v != in_reg) !spilled_intervals;
            ops := Fill (in_reg, preg) :: !ops
        end
    ) in_regs;
    (* Return the operations needed to assign registers for this instruction *)
    List.rev (Instr instr :: !ops)
    
(* Allocate registers for instructions list *)
let reg_alloc (regs : 'b list) (instrs : ('a, 'b) vreg_instr list) = 
    let intervals = ref (intervals_of_instrs instrs) in (* List of intervals for vregs ordered by starting position *)
    let active_intervals = ref [] in (* List of active intervals at current instruction *)
    let spilled_intervals = ref [] in
    List.concat (List.mapi (reg_alloc_instr regs intervals active_intervals spilled_intervals) instrs)
    
(* TODO (not actually needed for ARM):
    - allow in, out registers to be specified at each instructions instead of when declared (i.e. map from vreg to list of pregs)
      maybe not needed as a register is used twice only in the case of DEFTMP combined with USETEMP, but then we can generate optional moves
    - allow to specify two vregs to be allocated to the same preg
      example: add dest, src <=> dest = dest + src <=> out = in0
      possible solution: append optional move instruction
*)
