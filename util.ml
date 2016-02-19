(* ppc/util.ml *)

let lineno = ref 1                      (* Current line in input file *)

(* |copy n x = [x; x; ...; x]| with |n| copies of |x| *)
let rec copy n x = if n <= 0 then [] else x :: copy (n-1) x

(* |take n [x1; x2; ...] = [x1; x2; ...; xn]| *)
let rec take n =
  function
      [] -> []
    | x::xs -> if n = 0 then [] else x :: take (n-1) xs

(* |drop n [x1; x2; ...] = [x_{n+1}; x_{n+2}; ...]| *)
let rec drop n =
  function
      [] -> []
    | x::xs -> if n = 0 then x::xs else drop (n-1) xs

(* |can f x| is true if |f x| doesn't raise |Not_found| *)
let can f x = try f x; true with Not_found -> false

(* |make_hash n [(x1, y1); ...]| creates a hash table of size |n|
   that initially contains the pairs |(x1, y1)|, ... *)
let make_hash n ps = 
  let table = Hashtbl.create n in
  List.iter (function (x, y) -> Hashtbl.add table x y) ps;
  table

type 'a list_tree = Node of 'a * 'a list_tree list

type ('l, 'a, 'b) dag =
    | DagNode of 'a * ('l, 'a, 'b) dag list
    | DagEdge of 'b
    | DagRoot of 'l * ('l, 'a, 'b) dag
    
type ('a, 'b) either = Left of 'a | Right of 'b

let is_some opt = match opt with
    | Some _ -> true
    | None -> false
    
let is_none opt = match opt with
    | Some _ -> false
    | None -> true
    
let get_option opt = match opt with
    | Some value -> value
    | None -> failwith "Unable to get value from None."
    
let rec flatten_list_tree tree = match tree with
    | Node (value, children) -> List.concat (List.map flatten_list_tree children) @ [value]
    
let rec list_diff l0 l1 = match l0 with
    | [] -> []
    | x :: xs ->
        let xs' = list_diff xs l1 in
        if List.exists (fun p -> p = x) l1 then xs' else x :: xs'
        
let starts_with str pat = 
    if String.length str < String.length pat then false
    else String.sub str 0 (String.length pat) = pat
    
let ends_with str pat =
    if String.length str < String.length pat then false
    else String.sub str (String.length str - String.length pat) (String.length pat) = pat
