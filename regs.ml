(* ppcpi/regs.ml *)

open Target
open Print

(* |pool| -- list of allocatable registers *)
let pool = volatile @ stable

(* |temps| -- different order for allocating shared temps *)
let temps = stable @ volatile

(* |regmap| -- hash table giving refcount for each resister *)
let regmap = Util.make_hash 20 (List.map (fun r -> (r, ref 0)) pool)

(* |is_free| -- test if register is free *)
let is_free r =
  try !(Hashtbl.find regmap r) = 0 with Not_found -> false

(* |refcount| -- apply function to refcount cell *)
let refcount f r =
  try f (Hashtbl.find regmap r) with Not_found -> ()

(* |reserve_reg| -- reserve a register *)
let reserve_reg r = refcount incr r

(* |release_reg| -- release a register *)
let release_reg r = refcount decr r

(* |find_first| -- find first element of list passing a test *)
let rec find_first p =
  function
      [] -> raise Not_found
    | x::xs -> if p x then x else find_first p xs

(* |alloc| -- allocate register from specified set *)
let alloc set =
  try 
    let r = find_first is_free set in
    reserve_reg r; r
  with Not_found ->
    failwith "Sorry, I ran out of registers"

(* |alloc_reg| -- allocate any register *)
let alloc_reg () = alloc pool

(* |get_reg| -- replace R_any or R_temp by specific register *)
let get_reg r =
  match r with
      R_any -> alloc pool
    | R_temp -> alloc temps
    | _ -> reserve_reg r; r

(* |dump_regs| -- dump register state *)
let dump_regs () =
  let dump prf =
    let begun = ref false in
    List.iter (fun r -> 
        let x = !(Hashtbl.find regmap r) in
        if x <> 0 then begin
          if not !begun then begin
            prf "regs" []; begun := true
          end;
          prf " $=$" [fReg r; fNum x]
        end) pool in
  sprintf "$" [fExt dump]

(* |temp| -- data for temp variable *)
type temp =
  { t_id : int;                         (* Name *)
    t_refct : int ref;                  (* Number of references *)
    mutable t_reg : reg }               (* Allocated register *)

let ntemps = ref 0
let temptab = Hashtbl.create 129

(* |new_temp| -- create a temp variable *)
let new_temp c =
  incr ntemps; 
  let n = !ntemps in
  Hashtbl.add temptab n { t_id = n; t_refct = ref c; t_reg = R_none };
  n

(* |temp| -- get data for a temp variable *)
let temp n = Hashtbl.find temptab n

(* |inc_temp| -- increment refcount of a temp variable *)
let inc_temp n =
  let t = temp n in incr t.t_refct

(* |def_temp| -- specify register for a temp variable *)
let def_temp n r =
  let t = temp n in 
  t.t_reg <- r;
  decr t.t_refct;
  let v = Hashtbl.find regmap r in
  v := !v + !(t.t_refct) - 1

(* |temp_reg| -- fetch the register for a temp variable *)
let temp_reg n =
  let t = temp n in t.t_reg

(* |use_temp| -- use a temp variable *)
let use_temp n =
  let t = temp n in 
  decr t.t_refct; t.t_reg

(* |spill_temps| -- move temp variables to callee-save registers *)
let spill_temps regs =
  Hashtbl.iter (fun n t ->
      if !(t.t_refct) > 0 && t.t_reg <> R_none
          && List.mem t.t_reg regs then begin
        let r = alloc stable in
        Hashtbl.find regmap r := !(Hashtbl.find regmap t.t_reg);
        Hashtbl.find regmap t.t_reg := 0;
        move_reg r t.t_reg;
        t.t_reg <- r
      end)
    temptab

let init () = 
  ntemps := 0;
  Hashtbl.clear temptab;
  let zero x = (x := 0) in List.iter (refcount zero) pool

let get_regvars nregv =
  for i = 0 to nregv-1 do 
    reserve_reg (List.nth stable i)
  done 
