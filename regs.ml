(* ppcpi/regs.ml *)
open Print

(* |temp| -- data for temp variable *)
type temp =
  { t_id : int;                         (* Name *)
    t_refct : int ref;                  (* Number of references *)
  }

let ntemps = ref 0
let temptab = Hashtbl.create 129

(* |new_temp| -- create a temp variable *)
let new_temp c =
  incr ntemps; 
  let n = !ntemps in
  Hashtbl.add temptab n { t_id = n; t_refct = ref c};
  n

(* |temp| -- get data for a temp variable *)
let temp n = Hashtbl.find temptab n

(* |inc_temp| -- increment refcount of a temp variable *)
let inc_temp n =
  let t = temp n in incr t.t_refct

let init () = 
  ntemps := 0;
  Hashtbl.clear temptab
