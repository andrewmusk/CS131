type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let rec get_list listofrules nonterminal = match listofrules with
  | [] -> []
  | (left,right)::rest ->
    if left = nonterminal then right::(get_list rest nonterminal)
    else get_list rest nonterminal 
;;

let convert_grammar grammar = 
  fst grammar, (function nonterminal -> get_list (snd grammar) nonterminal)
;;

let rec tree_parser tree = match tree with
  | [] -> []
  | (Leaf leaf)::rest -> leaf::(tree_parser rest)
  | (Node (nonterminal, children))::rest -> (tree_parser children) @ (tree_parser rest)
;;

let parse_tree_leaves tree = tree_parser [tree] ;;

(* Match Maker *)

let rec matcher filter_function rules acceptor fragment =
  match rules with 
    | [] -> None
    | rule::rest -> (match rule_matcher filter_function rule acceptor fragment with
      | None -> matcher filter_function rest acceptor fragment
      | x -> x)
and rule_matcher filter_function rule acceptor fragment = 
  match rule with
    | [] -> acceptor fragment
    | symbol::rest -> ( match symbol with
      | N non_terminal -> matcher filter_function (filter_function non_terminal) (rule_matcher filter_function rest acceptor) fragment
      | T term -> terminal_matcher term rest filter_function acceptor fragment)
and terminal_matcher term rest filter_function acceptor fragment = 
      (match fragment with
        | [] -> None
        | head::tail -> if head=term then rule_matcher filter_function rest acceptor tail
        else None );;

let make_matcher gram acceptor fragment = (matcher (snd gram) ((snd gram)(fst gram)) acceptor fragment);;

(* Parser *)

(* Make Parser *)

let rec parser_matcher filter_function rules acceptor fragment =
  match rules with 
    | [] -> None
    | rule::rest -> (match parser_rule_matcher filter_function rule acceptor fragment with
      | None -> parser_matcher filter_function rest acceptor fragment
      | Some x -> Some (rule::x))
and parser_rule_matcher filter_function rule acceptor fragment = 
  match rule with
    | [] -> acceptor fragment
    | symbol::rest -> ( match symbol with
      | N non_terminal -> parser_matcher filter_function (filter_function non_terminal) (parser_rule_matcher filter_function rest acceptor) fragment
      | T term -> parser_terminal_matcher term rest filter_function acceptor fragment)
and parser_terminal_matcher term rest filter_function acceptor fragment = 
      (match fragment with
        | [] -> None
        | head::tail -> if head=term then parser_rule_matcher filter_function rest acceptor tail
        else None );;


let rec tree_builder tree rule_list = 
  match tree with 
    | [] -> (rule_list, []) 
    | current::rest -> (match (builder current rule_list) with 
      | (symbol,branch) -> (match tree_builder rest symbol with 
        | (top,new_part) -> (top, branch::new_part))) 

and builder tree rule_list =
  match tree with 
    | (T terminal) -> build_leaf rule_list terminal
    | (N non_terminal) -> build_node rule_list non_terminal

and build_leaf rule_list terminal = 
  if rule_list = [] then ([], Leaf terminal)
  else (rule_list, Leaf terminal)

and build_node rule_list non_terminal =
  match rule_list with 
    | [] -> ([], Node (non_terminal, [])) 
    | current::rest -> (match tree_builder current rest with 
      | (front,back) -> (front, Node (non_terminal, back)));;   

let get_head input= 
  (match input with 
    | [] -> None 
    | h::t -> Some h);;     

let make_parser gram1 fragment = 
let first = snd gram1 in
let first_rules = (snd gram1)(fst gram1) in 
let accept_empty suffix = match suffix with
    | [] -> Some []
    | _ -> None
in
let rule_set = parser_matcher first first_rules accept_empty fragment in
match rule_set with 
  | None -> None 
  | Some rule_list -> match rule_list with
    | [] -> None
    | x -> (match tree_builder [N (fst gram1)] x with 
      | (_,w) -> get_head w)
  ;;



