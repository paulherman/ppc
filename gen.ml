open Keiko
open Util
    
type pattern =
    | NonTerm of string
    | Term of Keiko.inst
    | PNode of Keiko.inst * pattern list
             
type ('a, 'b) either = Left of 'a | Right of 'b

type 'a instr_gen = ('a, Keiko.inst) either list -> 'a

type 'a rule = Rule of string * pattern * int * 'a instr_gen

type 'a cost_map = (string, int * 'a rule) Hashtbl.t

type 'a dp = DP of inst * ('a dp) list * 'a cost_map

let string_of_op op = match op with
    | Plus -> "+" 
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Eq -> "="
    | Uminus -> "-"
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||"
    | Not -> "!"
    | PlusA -> "A+"
    | Lsl-> "<<"
    | Lsr -> ">>"
    | Asr -> ">>>"
    | BitAnd -> "&"
    | BitOr -> "|"
    | BitNot-> "~"

let string_of_inst instr = match instr with
    | CONST k -> "CONST " ^ (string_of_int k)
    | GLOBAL g -> "GLOBAL " ^ g
    | LOCAL l -> "LOCAL " ^ (string_of_int l)
    | REGVAR r -> "REGVAR " ^ (string_of_int r)
    | LOADC -> "LOADC"
    | LOADW -> "LOADW"
    | STOREC -> "STOREC"
    | STOREW -> "STOREW"
    | ARG a -> "ARG " ^ (string_of_int a)
    | SLINK -> "SLINK"
    | PCALL n -> "PCALL " ^ (string_of_int n)
    | RESULTW -> "RESULTW"
    | MONOP o -> "MONOP " ^ (string_of_op o)
    | BINOP o -> "BINOP " ^ (string_of_op o)
    | BOUND -> "BOUND"
    | NCHECK -> "NCHECK"
    | LABEL l -> "LABEL " ^ (string_of_int l)
    | JUMP l -> "JUMP " ^ (string_of_int l)
    | JUMPC (o, l) -> "JUMPC " ^ (string_of_int l) ^ " " ^ (string_of_op o)
    | JCASE (ls, l) -> "JCASE " ^ (string_of_int l) ^ " (" ^ (String.concat ", " (List.map string_of_int ls)) ^ ")"
    | TEMP l -> "TEMP " ^ (string_of_int l)
    | DEFTMP l -> "DEFTMP " ^ (string_of_int l)

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
    | JUMPC _, JUMPC _ -> true
    | JCASE _, JCASE _ -> true
    | TEMP _, TEMP _ -> true
    | DEFTMP _, DEFTMP _ -> true
    | _, _ -> false

let rec string_of_pattern pat = match pat with
    | NonTerm nt -> nt
    | Term t -> string_of_inst t
    | PNode (t, children) -> (string_of_inst t) ^ " <" ^ (String.concat ", " (List.map string_of_pattern children)) ^ ">"

let print_rule rule = match rule with Rule (nonterm, pat, cost, gen) -> print_endline ("Rule: " ^ nonterm ^ " " ^ string_of_int cost ^ " <" ^ (string_of_pattern pat) ^ ">")

let rec print_dp dp = match dp with DP (instr, children, costs) -> (print_string ("DP " ^ string_of_inst instr); Hashtbl.iter (fun k v -> print_string (k ^ " ")) costs; print_newline (); List.iter print_dp children)

let sum_list = List.fold_left (+) 0

let rule_compare (Rule (nt0, pat0, cost0, gen0)) (Rule (nt1, pat1, cost1, gen1)) =
    let pat_value rule = match rule with
        | NonTerm _ -> 2
        | Term _ -> 1
        | PNode _ -> 0
    in compare (pat_value pat0) (pat_value pat1)
    
let option_is_none opt = match opt with
    | Some _ -> false
    | None -> true
    
let option_get opt = match opt with
    | Some v -> v
    | None -> raise (Invalid_argument "option_get")

let rec pattern_cost pat tree = match tree with
    DP (instr, children, costs) -> match pat with
        | NonTerm nonterm -> if Hashtbl.mem costs nonterm then Some (fst (Hashtbl.find costs nonterm)) else None
        | Term term -> if same_instr instr term then Some 0 else None
        | PNode (term, pats) -> if same_instr term instr && List.length pats = List.length children
                                then let children_costs = List.map2 pattern_cost pats children in 
                                    if List.exists option_is_none children_costs then None 
                                    else Some (sum_list (List.map option_get children_costs))
                                else None

let update_cost costs rule nonterm cost = if not (Hashtbl.mem costs nonterm) then Hashtbl.add costs nonterm (cost, rule)
                                          else match Hashtbl.find costs nonterm with (prev_cost, prev_rule) ->
                                              if prev_cost > cost then Hashtbl.add costs nonterm (cost, rule) else ()

let apply_rule tree rule = match rule, tree with Rule (nonterm, pat, rule_cost, gen), DP (instr, children, costs) -> 
                           match pattern_cost pat tree with
                                 None -> ()
                               | Some total_cost -> update_cost costs rule nonterm (rule_cost + total_cost)

let rec apply_rules rules tree = match tree with DP (instr, children, costs) -> List.iter (apply_rules rules) children; List.iter (apply_rule tree) rules

let rec dp_of_optree num_rules tree = match tree with
    Util.Node (instr, children) -> DP (instr, List.map (dp_of_optree num_rules) children, Hashtbl.create num_rules)

    
let rec get_pattern_args pat dp = match dp with DP (instr, children, costs) -> match pat with
    | Term term -> [Right instr]
    | NonTerm nonterm -> [Left (emit_code nonterm dp)]
    | PNode (term, pats) -> (Right instr) :: List.concat (List.map2 get_pattern_args pats children)
and emit_code symbol dp = match dp with DP (instr, children, costs) ->
                          match Hashtbl.find costs symbol with (cost, rule) -> 
                          match rule with Rule (nonterm, pat, cost, gen) ->
                          gen (get_pattern_args pat dp)
                          
let dump_dp dp = match dp with
    DP (instr, children, costs) -> Hashtbl.iter (fun k v -> print_string k; print_endline "") costs
    
let translate symbol rules ir = 
    let sorted_rules = List.sort rule_compare rules in
    let dp = dp_of_optree (List.length sorted_rules) ir in
    apply_rules sorted_rules dp; 
    emit_code symbol dp
