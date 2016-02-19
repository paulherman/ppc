open Keiko
open Util
open Print
    
type pattern =
    | NonTerm of string
    | Term of Keiko.inst
    | GTerm of Keiko.inst * (Keiko.inst -> bool)
    | GPNode of Keiko.inst * (Keiko.inst -> bool) * pattern list
    | PNode of Keiko.inst * pattern list
             
type ('a, 'b) either = Left of 'a | Right of 'b

type 'a instr_gen = ('a, Keiko.inst) either list -> 'a list

type 'a rule = Rule of string * pattern * int * 'a instr_gen

type 'a cost_map = (string, int * 'a rule) Hashtbl.t

type 'a dp = DP of inst * ('a dp) list * 'a cost_map

let same_instr left right = match left, right with
    | CONST _, CONST _ -> true
    | GLOBAL _, GLOBAL _ -> true
    | LOCAL _, LOCAL _ -> true
    | REGVAR _, REGVAR _ -> true
    | LOADC, LOADC -> true
    | LOADW, LOADW -> true
    | STOREC, STOREC -> true
    | STOREW, STOREW -> true
    | ARG _, ARG _ -> true
    | SLINK, SLINK -> true
    | PCALL _, PCALL _ -> true
    | RESULTW, RESULTW -> true
    | MONOP o, MONOP p -> o = p
    | BINOP o, BINOP p -> o = p
    | BOUND, BOUND -> true
    | NCHECK, NCHECK -> true
    | LABEL _, LABEL _ -> true
    | JUMP _, JUMP _ -> true
    | JUMPC (o, _), JUMPC (p, _) -> o = p
    | JCASE _, JCASE _ -> true
    | TEMP _, TEMP _ -> true
    | DEFTMP _, DEFTMP _ -> true
    | LINE _, LINE _ -> true
    | _, _ -> false

let rec string_of_pattern pat = match pat with
    | NonTerm nt -> nt
    | Term t -> string_of_inst t
    | PNode (t, children) -> (string_of_inst t) ^ " <" ^ (String.concat ", " (List.map string_of_pattern children)) ^ ">"
    | GTerm (t, g) -> string_of_inst t
    | GPNode (t, g, children) -> (string_of_inst t) ^ " <" ^ (String.concat ", " (List.map string_of_pattern children)) ^ ">"

let print_rule rule =
    match rule with
        | Rule (nonterm, pat, cost, gen) ->
            print_endline ("Rule: " ^ nonterm ^ " " ^ string_of_int cost ^ " <" ^ (string_of_pattern pat) ^ ">")

let rec print_dp depth dp =
    let indent = String.concat "" (copy depth "  ") in
    match dp with
        | DP (instr, children, costs) ->
            print_string indent;
            print_string (string_of_inst instr);
            print_string ":";
            Hashtbl.iter (fun k v -> print_string (" " ^ k)) costs;
            print_endline "";
            List.iter (print_dp (depth + 1)) children

let rule_compare (Rule (nt0, pat0, cost0, gen0)) (Rule (nt1, pat1, cost1, gen1)) =
    let pat_value rule = match rule with
        | NonTerm _ -> 2
        | Term _ -> 1
        | GTerm _ -> 1
        | PNode _ -> 0
        | GPNode _ -> 0
    in compare (pat_value pat0) (pat_value pat1)
    

let rec pattern_cost pat tree = match tree with
    DP (instr, children, costs) -> match pat with
        | NonTerm nonterm ->
            if Hashtbl.mem costs nonterm then Some (fst (Hashtbl.find costs nonterm)) else None
        | GTerm (term, guard) ->
            if same_instr instr term && guard instr then Some 0 else None
        | Term term ->
            if same_instr instr term then Some 0 else None
        | GPNode (term, guard, pats) ->
            if same_instr term instr && guard instr && List.length pats = List.length children
            then let children_costs = List.map2 pattern_cost pats children in 
                if List.exists is_none children_costs then None 
                else Some (sum_list (List.map get_option children_costs))
            else None
        | PNode (term, pats) ->
            if same_instr term instr && List.length pats = List.length children
            then let children_costs = List.map2 pattern_cost pats children in 
                if List.exists is_none children_costs then None 
                else Some (sum_list (List.map get_option children_costs))
            else None

let update_cost costs rule nonterm cost = if not (Hashtbl.mem costs nonterm) then Hashtbl.add costs nonterm (cost, rule)
                                          else let (prev_cost, prev_rule) = Hashtbl.find costs nonterm in
                                              if prev_cost > cost then Hashtbl.add costs nonterm (cost, rule) else ()

let apply_rule tree rule =
    match rule, tree with
        Rule (nonterm, pat, rule_cost, gen), DP (instr, children, costs) -> 
            match pattern_cost pat tree with
                | None -> ()
                | Some total_cost -> update_cost costs rule nonterm (rule_cost + total_cost)

let rec apply_rules rules tree = 
    match tree with
        | DP (instr, children, costs) ->
            List.iter (apply_rules rules) children;
            List.iter (apply_rule tree) rules

let rec dp_of_optree num_rules tree =
    match tree with
        Util.Node (instr, children) ->
            DP (instr, List.map (dp_of_optree num_rules) children, Hashtbl.create num_rules)

    
let rec get_pattern_args pat dp = match dp with DP (instr, children, costs) -> match pat with
    | Term term -> [Right instr]
    | GTerm (term, guard) -> [Right instr]
    | NonTerm nonterm -> List.map (fun x -> Left x) (emit_code nonterm dp)
    | GPNode (term, guard, pats) -> (Right instr) :: List.concat (List.map2 get_pattern_args pats children)
    | PNode (term, pats) -> (Right instr) :: List.concat (List.map2 get_pattern_args pats children)
and emit_code symbol dp =
    match dp with
        | DP (instr, children, costs) ->
            if Hashtbl.mem costs symbol then
                let (total_cost, Rule (nonterm, pat, cost, gen)) = Hashtbl.find costs symbol in
                gen (get_pattern_args pat dp)
            else failwith ("Unable to tile IR tree with assembly instructions for nonterminal " ^ symbol ^ " for instruction " ^ string_of_inst instr ^ ".")
    
let translate symbol rules ir = 
    let sorted_rules = List.sort rule_compare rules in
    let dp = dp_of_optree (List.length sorted_rules) ir in
    apply_rules sorted_rules dp;
    try emit_code symbol dp with Not_found -> failwith "Unable to tile IR tree with assembly instructions."
