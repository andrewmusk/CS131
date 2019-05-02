let rec contains l i = match l with
  | [] -> false
  | h :: t -> (if h = i then true else contains t i);;

(* 1. Write a function subset a b that returns true iff a⊆b*)
let rec subset a b = match a with
  | [] -> true
  | h :: t -> (if not (contains b h) then false else subset t b);;

(* 2. Write a function equal_sets a b that returns true iff the represented sets are equal.*)

let equal_sets a b = 
  subset a b && subset b a;;

(* 3. Write a function set_union a b that returns a list representing a∪b*)

let rec set_union a b = match a with 
  | [] -> b
  | h :: t -> (if contains b h then set_union t b else set_union t (h::b));;

(* 4. Write a function set_intersection a b that returns a list representing a∩b. *)

let rec set_intersection a b = match a with
  | [] -> []
  | h :: t -> (if contains b h then h::(set_intersection t b) else set_intersection t b);;

(* 5. Write a function set_diff a b that returns a list representing a−b, that is, the set of all members of a that are not also members of b.*)
let rec set_diff a b = 
  if set_intersection a b  = [] then a 
  else match a with
    | [] -> []
    | h :: t -> (if contains b h then set_diff t b else h::(set_diff t b));;

(* 6. Write a function computed_fixed_point eq f x that returns the computed fixed point for f with respect to x, 
  assuming that eq is the equality predicate for f's domain. A common case is that eq will be (=), that is, 
the builtin equality predicate of OCaml; but any predicate can be used. If there is no computed fixed point, 
your implementation can do whatever it wants: for example, it can print a diagnostic, or go into a loop, 
or send nasty email messages to the user's relatives.*)

let rec computed_fixed_point eq f x =
  if eq x (f x) then x 
  else computed_fixed_point eq f (f x)
;;

(* 7. Write a function filter_reachable g that returns a copy of the grammar g with all unreachable rules removed. 
This function should preserve the order of rules: that is, all rules that are returned should be in the same order as the rules in g.*)
type ('a, 'b) symbol =
  | N of 'a
  | T of 'b
;; 

let rec get_nonterminals rule_list =
  match rule_list with
  | [] -> []
  | T head :: rest -> get_nonterminals rest
  | N head :: rest -> head :: (get_nonterminals rest)
;;

let left (x, _) = x;;
let right (_, x) = x;;
let add_to_list reachable_nonterminals rule = set_union reachable_nonterminals (get_nonterminals (right rule));;

let rec get_nonterminal_list reachable_nonterminals list_of_rules =
  match list_of_rules with
  | [] -> reachable_nonterminals
  | rule :: rest_rules -> 
  if not (contains reachable_nonterminals (left rule)) 
    then get_nonterminal_list reachable_nonterminals rest_rules
    else get_nonterminal_list (add_to_list reachable_nonterminals rule) rest_rules
;;

let rec get_all_reachable_nonterminals reachable_nonterminals list_of_rules =
  let initial = get_nonterminal_list reachable_nonterminals list_of_rules in
  let check = get_nonterminal_list initial list_of_rules in
    if equal_sets initial check then initial else get_all_reachable_nonterminals check list_of_rules
;;

let filter_reachable grammar =
  let start_nonterminal = left grammar in 
  let rules = right grammar in 
  (start_nonterminal, let all_reachable_nonterminals = (get_all_reachable_nonterminals [start_nonterminal] rules) in
  List.filter (fun x -> contains all_reachable_nonterminals (left x)) rules)
;;
