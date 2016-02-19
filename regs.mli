(* ppcpi/regs.mli *)

(* |init| -- initialise register state *)
val init : unit -> unit

(* Temps *)

(* |new_temp| -- allocate a temp with specified reference count *)
val new_temp : int -> int

(* |inc_temp| -- increment refcount of a temp variable *)
val inc_temp : int -> unit

