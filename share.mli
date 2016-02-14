(* ppc/share.mli *)

(* The function |traverse| transforms a list of optrees by finding 
   common subexpressions; for each of them, it allocates a temp with
   an initializing assignment, then replaces all occurrences of the
   subexpression with a use of the temp.  Procedure calls are also moved
   to the top level by a similar mechanism. *)

(* |traverse| -- find common subexpressions in a procedure body *)
val traverse : Keiko.optree list -> Keiko.optree list
