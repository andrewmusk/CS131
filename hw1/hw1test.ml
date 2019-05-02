let my_subset_test0 = subset [1;1;1] [1]
let my_subset_test1 = subset [] []
let my_subset_test1 = subset [1;2;3;4;5;6;7;8] [1;2;3;4;5;6;7;8;9]

let my_equal_sets_test0 = equal_sets [1] [1;1;1;1;1;1]
let my_equal_sets_test1 = equal_sets [] []
let my_equal_sets_test2 = not (equal_sets [] [2;3])

let my_set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [1;2] [1;3;3;3;3]) [1;2;3]


let my_set_intersection_test0 =
  equal_sets (set_intersection [] []) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;3;3;3;3] [1;2;3]) [3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;1;1;1;1] [1;1;1;1]) [1]


let my_set_diff_test0 = equal_sets (set_diff [1;3;3;3;3] [1;4;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [1;1;1;1;2;3;4;5] [1]) [2;3;4;5]
let my_set_diff_test2 = equal_sets (set_diff [1;2;3;3;3;3] []) [1;2;3]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> 0.) 1. = 0.
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> ((x *. x) -. (3.0 *. x) +. 4.0)) 1.0 = 2.0



type sports = Phrase | Sport | Item | Stadium

let random = [
Phrase, [N Sport; N Item];
Sport, [T "Cricket"];
Sport, [T "Baseball"];
Item, [T "Bat"];
Item, [T "Ball"];
Stadium, [T "At&t Park"];
]

let my_filter_reachable_test0 = filter_reachable (Phrase, random) = 
(Phrase, [
Phrase, [N Sport; N Item];
Sport, [T "Cricket"];
Sport, [T "Baseball"];
Item, [T "Bat"];
Item, [T "Ball"]
])

let my_filter_reachable_test1 = filter_reachable (Sport, random) = 
(Sport, [
Sport, [T "Cricket"];
Sport, [T "Baseball"];
])