let acceptor = function
  | h::t -> None
  | x -> Some x

type nonterminals = 
  | Schedule | OnCampus | OffCampus | Class | Lunch | Leisure | Type | Sport

let grammar = 
  (Schedule, 
   function 
  | Schedule ->
    [[N OnCampus];
     [N OffCampus];
     [N OnCampus; N OffCampus; N OnCampus];
     [N OffCampus; N OnCampus]]
  | OnCampus ->
    [[N Class];
     [N Lunch]]
  | Class ->
    [[T "Math"; N Type];
     [T "Physics"]]
  | Lunch ->
    [[T "Rubios"];
     [T "Panda"]]
  | Type ->
    [[T "31A"];
     [T "31B"]]
  | OffCampus ->
    [[N Sport];
    [N Leisure; N Lunch];
    [N Leisure]]
  | Sport ->
    [[T "Soccer"];
     [T "Tennis"]]
  | Leisure ->
    [[T "Movie"];
     [T "Hike"]])

let make_matcher_test = 
  ((make_matcher grammar acceptor ["Math"; "31A"; "Hike"; "Rubios"; "Physics"]) = Some [])

let make_matcher_test = 
  ((make_matcher grammar acceptor ["Wolfgang"]) = None)


(*Checking order is preserved in rules*)
let make_parser_test = 
  ((make_parser grammar ["Hike"; "Rubios"]) = 
  Some
   (Node (Schedule,
     [Node (OffCampus, [Node (Leisure, [Leaf "Hike"]); Node (Lunch, [Leaf "Rubios"])])])))

(*Checking can handle more complex uneven tree structure*)
let make_parser_test = 
  ((make_parser grammar ["Math"; "31A"]) = 
  Some
   (Node (Schedule,
     [Node (OnCampus, [Node 
        (Class, [Leaf "Math" ; Node (Type, [Leaf "31A"])])]
   )]))) 

(*checking empty tree *)
let make_parser_test = 
  ((make_parser grammar ["Wolfgang"]) = None )


