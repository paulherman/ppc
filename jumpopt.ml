# 1 "jumpopt.mlp"
(* ppcpi/jumpopt.mlp *)

open Keiko

(* Disjoint sets of labels *)

type labdata = 
    LabDef of labrec                    (* An extant label *)
  | Equiv of codelab                    (* A label that's been merged *)

and labrec = 
  { y_id : codelab;                     (* Name of the label *)
    y_refct : int ref }                 (* Reference count *)

let label_tab = Hashtbl.create 257

(* get_label -- find or create a label *)
let get_label x =
  try Hashtbl.find label_tab x with
    Not_found -> 
      let y = LabDef { y_id = x; y_refct = ref 0 } in
      Hashtbl.add label_tab x y; y

(* find_label -- find equivalent of a label *)
let rec find_label x =
  match get_label x with
      LabDef y -> y
    | Equiv x' -> find_label x'

let rename x = let y = find_label x in y.y_id

let ref_count x = let y = find_label x in y.y_refct

(* same_lab -- test if two labels are equal *)
let same_lab x1 x2 =
  let y1 = find_label x1 and y2 = find_label x2 in
  y1.y_id = y2.y_id

(* equate -- make two labels equal *)
let equate x1 x2 =
  let y1 = find_label x1 and y2 = find_label x2 in
  if y1.y_id = y2.y_id then failwith "equate";
  y2.y_refct := !(y1.y_refct) + !(y2.y_refct);
  Hashtbl.add label_tab y1.y_id (Equiv y2.y_id)  

(* do_refs -- call function on each label *)
let do_refs f =
  function
      (Node (JUMP x, [])) -> f (ref_count x)
    | (Node (JUMPC (w, x), [_;  _])) -> f (ref_count x)
    | (Node (JCASE (labs, def), [_])) -> 
        List.iter (fun x -> f (ref_count x)) labs;
        f (ref_count def)
    | _ -> ()

(* rename_labs -- replace each label by its equivalent *)
let rename_labs =
  function
      (Node (LABEL x, [])) -> (Node (LABEL (rename x), []))
    | (Node (JUMP x, [])) -> (Node (JUMP (rename x), []))
    | (Node (JUMPC (w, x), [t1;  t2])) -> (Node (JUMPC (w, rename x), [t1;  t2]))
    | (Node (JCASE (labs, def), [t1])) -> (Node (JCASE (List.map rename labs, rename def), [t1]))
    | t -> t

let optstep changed code =
  let ch = ref true in

  let replace n inserted = 
    changed := true; ch := true;
    let deleted = Util.take n !code in
    List.iter (do_refs decr) deleted;
    List.iter (do_refs incr) inserted; 
    code := inserted @ Util.drop n !code in

  let delete n = replace (n+1) (Util.take n !code) in

  while !ch do
    ch := false;
    match !code with
        (Node (JUMP lab1, [])) :: (Node (LABEL lab2, [])) :: _ -> 
          (* Remove a jump to the next instruction *)
          if same_lab lab1 lab2 then delete 0
      | (Node (JUMP lab1, [])) :: (Node (LINE n, [])) :: (Node (LABEL lab2, [])) :: _ ->
          (* Keep a potentially useful line number *)
          replace 3 [(Node (JUMP lab1, [])); (Node (LABEL lab2, [])); (Node (LINE n, []))]
      | (Node (JUMP lab, [])) :: _ :: _ -> 
          (* Eliminate dead code *)
          delete 1
      | (Node (JUMPC (w, lab1), [t1;  t2])) :: (Node (JUMP lab2, [])) :: (Node (LABEL lab3, [])) :: _ ->
          (* Simplify a jump over a jump *)
          if same_lab lab1 lab3 then
            replace 2 [(Node (JUMPC (negate w, lab2), [t1;  t2]))]
      | (Node (LABEL lab1, [])) :: (Node (JUMP lab2, [])) :: _ -> 
          (* One jump leads to another *)
          if not (same_lab lab1 lab2) then begin
            delete 0; equate lab1 lab2
          end
      | (Node (LABEL lab1, [])) :: (Node (LABEL lab2, [])) :: _ ->
          (* Merge identical labels *)
          delete 0; equate lab1 lab2
      | (Node (LABEL lab, [])) :: _ ->
          (* Delete unused labels *)
          if !(ref_count lab) = 0 then delete 0

      (* Tidy up line numbers *)
      | (Node (LINE m, [])) :: (Node (LINE n, [])) :: _ ->
          delete 0
      | (Node (LINE n, [])) :: (Node (LABEL lab, [])) :: _ ->
          replace 2 [(Node (LABEL lab, [])); (Node (LINE n, []))]
      | (Node (LINE n, [])) :: (Node (JUMP lab, [])) :: _ ->
          replace 2 [(Node (JUMP lab, [])); (Node (LINE n, []))]
      | (Node (LINE n, [])) :: [] ->
          delete 0

      | _ -> ()
  done

let optimise prog =
  Hashtbl.clear label_tab;
  let init = prog in
  List.iter (do_refs incr) init;
  let buf1 = ref init and buf2 = ref [] in
  let changed = ref true in
  while !changed do
    changed := false;
    while !buf1 <> [] do
      optstep changed buf1;
      if !buf1 <> [] then begin
        buf2 := List.hd !buf1 :: !buf2;
        buf1 := List.tl !buf1
      end
    done;
    buf1 := List.rev !buf2;
    buf2 := []
  done;
  List.map rename_labs !buf1

