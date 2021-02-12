type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules
(*test question 1*)
let test_grammar1 = convert_grammar awksub_grammar


(*Test Cases Provided*)

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

(* An example grammar for a small subset of Awk.
   This grammar is not the same as Homework 1; it is
   instead the same as the grammar under
   "Theoretical background" above.  *)

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test0 =
  ((make_matcher awkish_grammar accept_all ["ouch"]) = None)

let test1 =
  ((make_matcher awkish_grammar accept_all ["9"])
   = Some [])

let test2 =
  ((make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some ["+"])

let test3 =
  ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None)

(* This one might take a bit longer.... *)
let test4 =
 ((make_matcher awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some [])

let test5 =
  (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
   = [3; 4; 5])

let small_awk_frag = ["$"; "1"; "++"; "-"; "2"]

let test6 =
  ((make_parser awkish_grammar small_awk_frag)
   = Some (Node (Expr,
		 [Node (Term,
			[Node (Lvalue,
			       [Leaf "$";
				Node (Expr,
				      [Node (Term,
					     [Node (Num,
						    [Leaf "1"])])])]);
			 Node (Incrop, [Leaf "++"])]);
		  Node (Binop,
			[Leaf "-"]);
		  Node (Expr,
			[Node (Term,
			       [Node (Num,
				      [Leaf "2"])])])])))
let test7 =
  match make_parser awkish_grammar small_awk_frag with
    | Some tree -> parse_tree_leaves tree = small_awk_frag
    | _ -> false

(*END Provided tests*)

(*
(*Redo test 4 to make it suit my needs*)
(*It takes a very long time but it works*)
let test8 =
  make_parser awkish_grammar
      ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
       "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
       "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; ")"]
*)

(*Creating own grammar*)

let opposite_empty_suffix = function
	| [] -> None
	| x -> Some x

type giant_nonterminals =
  | Sentence | NounAdj | Noun | Verb | Adjective

let giant_grammar = 
    (Sentence,
    function
    | Sentence -> 
        [[N NounAdj; N Verb; N NounAdj]; 
            [N NounAdj; N Verb]]
    | NounAdj ->
        [[N Adjective; N Noun];
            [N Noun]]
    | Noun ->
        [[T "Blood"];
            [T "Beans"];
            [T "Sky"; T "Giant"];
            [T "Farmer"; T "Jack"]]
    | Verb ->
        [[T "loves"];
            [T "smells"]]
    | Adjective ->
        [[T "scared"];
            [T "excited"];
            [T "Magical"]])

let sample_matcher_frag = ["excited"; "Sky"; "Giant"; "smells"; "Blood"]

let sample_parser_frag = ["scared"; "Farmer"; "Jack"; "loves"; "Magical"; "Beans"]

let make_matcher_test = 
	((make_matcher giant_grammar opposite_empty_suffix sample_matcher_frag)
		= Some ["Blood"])

let make_parser_test = match make_parser giant_grammar sample_matcher_frag with
	| Some tree -> parse_tree_leaves tree = sample_matcher_frag
	| current -> false
