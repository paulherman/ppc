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

(* Augment instruction DAG with virtual registers from an infinite pool *)
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
        Hashtbl.replace intervals out_reg (count, count - 1, pref_regs);
        List.iter (fun in_reg ->
            let (s, e, pregs) = Hashtbl.find intervals in_reg in
            Hashtbl.replace intervals in_reg (s, count, pregs)) in_regs
    ) instrs;
    let intervals_list = List.filter (fun (v, ps, s, e) -> e >= s) (Hashtbl.fold (fun vreg (s, e, ps) is -> (vreg, ps, s, e) :: is) intervals []) in
    List.sort (fun (r0, ps0, s0, e0) (r1, ps1, s1, e1) -> compare s0 s1) intervals_list

let pregs_of_active_intervals intervals = List.map (fun (vreg, preg, pregs, s, e) -> preg) intervals

(* Assign the vreg to some preg, possibly by spilling some other register *)
let preg_of_vreg ops pregs unassignable unspillable active_intervals spilled_intervals vreg s e = 
    let sorted_intervals = List.sort (fun (v0, p0, ps0, s0, e0) (v1, p1, ps1, s1, e1) -> compare e0 e1) !active_intervals in
    let active_pregs = pregs_of_active_intervals (List.filter (fun (_, _, _, _, e0) -> e0 > e) !active_intervals) in
    let empty_regs = list_diff pregs active_pregs in
    (* Search for an empty register *)
    if empty_regs != [] then begin
        let preg = List.hd empty_regs in
        active_intervals := (vreg, preg, pregs, s, e) :: !active_intervals;
        preg
    end
    (* Try to spill a register not needed for the current instr *)
    else begin
        let preg = ref None in
        List.iter (fun (v, p, ps, s, e) -> 
            if List.mem v unspillable && not (List.mem p unassignable) then preg := Some ((v, p, ps, s, e))
        ) sorted_intervals;
        if is_some !preg then
            let (sv, sp, sps, ss, se) = get_option !preg in
            ops := Spill (sv, sp) :: !ops;
            active_intervals := (vreg, sp, pregs, s, e) :: List.filter (fun (v, _, _, _, _) -> v = sv) !active_intervals;
            sp
        else begin
            (* On CISC platforms, try to move a register needed for the current instr to another register and then spill *)
            failwith "Try to move a register needed for the current instr to another register and then spill"
        end
    end
    

let string_of_interval = fun (v, _, s, e) -> string_of_int v ^ ": " ^ string_of_int s ^ " -> " ^ string_of_int e
let print_interval i = print_endline (string_of_interval i)

(* Allocate registers for one instruction *)   
let reg_alloc_instr regs intervals active_intervals spilled_intervals count instr =
    let (instr, out_reg, in_regs, _, trash_regs) = instr in
    let ops = ref [] in
    (* Remove closed intervals *)
    active_intervals := List.filter (fun (vreg, preg, pregs, s, e) -> e >= count) !active_intervals;
    print_endline ("Number #" ^ string_of_int count);
    List.iter print_interval (List.map (fun (v, _, _, s, e) -> (v, [], s, e)) !active_intervals);
    (* Spill or move trashed registers *)
    List.iter (fun preg ->
        if List.exists (fun (v, p, ps, s, e) -> preg = p) !active_intervals then begin
            let (vreg, preg, pregs, s, e) = List.find (fun (v, p, ps, s, e) -> preg = p) !active_intervals in
            if e > count then begin
                active_intervals := List.filter (fun (v, p, ps, s, e) -> preg != p) !active_intervals;
                let preg = preg_of_vreg ops regs trash_regs in_regs active_intervals spilled_intervals vreg s e in
                ops := Move (vreg, preg) :: !ops
            end
        end
    ) trash_regs;
    (* Allocate the out register *)
    if !intervals != [] then begin
        let (out_reg, pref_regs, s, e) = List.hd !intervals in
        if s = count then begin
            print_endline ("Allocate " ^ string_of_int out_reg);
            let unspillable = in_regs in
            let preg = preg_of_vreg ops pref_regs [] unspillable active_intervals spilled_intervals out_reg s e in
            ops := Assign (out_reg, preg) :: !ops;
            intervals := List.tl !intervals
        end;
    end;
    (* Fill back spilled in registers *)
    List.iter (fun in_reg ->
        if not (List.exists (fun (vreg, preg, pregs, s, e) -> in_reg = vreg) !active_intervals) then begin
            let (in_reg, in_pref_regs, s, e) = List.find (fun (vreg, ps, s, e) -> vreg = in_reg) !spilled_intervals in
            let unspillable = out_reg :: in_regs in
            let preg = preg_of_vreg ops in_pref_regs trash_regs unspillable active_intervals spilled_intervals in_reg s e in
            spilled_intervals := List.filter (fun (v, ps, s, e) -> v != in_reg) !spilled_intervals;
            ops := Fill (in_reg, preg) :: !ops
        end
    ) in_regs;
    (* Return the operations needed to assign registers for this instruction *)
    List.rev (Instr instr :: !ops)
    
(* Allocate registers for instructions list *)
let reg_alloc (regs : 'b list) (instrs : ('a, 'b) vreg_instr list) = 
    let intervals = ref (intervals_of_instrs instrs) in (* List of intervals for vregs ordered by starting position *)
    let active_intervals = ref [] in (* List of active intervals at current instruction *)
    let spilled_intervals = ref [] in (* List of currently spilled intervals *)
    List.iter print_interval !intervals;
    List.concat (List.mapi (reg_alloc_instr regs intervals active_intervals spilled_intervals) instrs)
    
