type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(*convert_grammar helper function*)
let rec rewrite gram1 gram2 x = match gram1 with [] -> gram2
	| elem1 :: g1rest -> match elem1 with (term1, term2) -> if term1 =  x 
					        then rewrite g1rest (gram2@[term2]) x
					        else rewrite g1rest gram2 x
(*Question 1, convert_grammar*)
let convert_grammar gram1 = match gram1 with (terms, rules) -> (terms, rewrite rules [])

(*Question 2 parse_tree_leaves*)

let rec parse_tree_leaves = function Leaf endingNode -> [endingNode]
  | Node (curNode, curHead::curTail) -> (parse_tree_leaves curHead) @ (parse_down curTail)
  | curNode -> [] and parse_down = function [] -> []
  | (realHead::realTail) -> (parse_tree_leaves realHead) @ (parse_down realTail)


(*Question 3 Make_matched*)
(*https://codereview.stackexchange.com/questions/15558/is-this-use-of-let-and-in-ocaml-poor-style*)
let rec branch acceptor_returned syntax accept frag = match syntax with [] -> None
	| realHead::realTail -> (match nodes acceptor_returned realHead accept frag with
		| None -> branch acceptor_returned realTail accept frag
		| x -> x)
and nodes acceptor_returned syn accept frag = match syn with [] -> accept frag
	| elem1 -> (match frag with [] -> None
		| curHead::curTail -> (match syn with [] -> None
			| (T term)::right_side -> if curHead = term then (nodes acceptor_returned right_side accept curTail) else None
			| (N nterm)::right_side -> (branch acceptor_returned (acceptor_returned nterm) (nodes acceptor_returned right_side accept) frag)));;
				
let make_matcher gram accept frag = ((branch (snd gram) ((snd gram)(fst gram)) accept frag));;

(*Question 4 Make_parser*)

let rec extra_parser sym syntax proper_form accep tree frag= 
    match syntax with [] -> None
    | right_side::rest ->
      let rec head_parser right_side proper_form accep tree frag = 
        match right_side with 
        | [] -> accep tree frag
        | first_sym::rest -> 
          match first_sym with 
          | T term ->
            if List.length frag = 0 then
              None 
            else if term = List.hd frag then 
              if (List.length frag) = 1 then
                head_parser rest proper_form accep tree []
              else
                head_parser rest proper_form accep tree (List.tl frag)
            else
              None
          | N nterm -> 
            extra_parser nterm (proper_form nterm) proper_form (head_parser rest proper_form accep) tree frag
      in
      let result = head_parser right_side proper_form accep ((sym, right_side)::tree) frag in
      if result = None then
        extra_parser sym rest proper_form accep tree frag
      else 
        result

let rec make_parser gram frag = 
  let in_empty tree frag = 
  match frag with [] -> Some tree
  | curNode -> None
  in
  let total_function = snd gram in
  let start_sym = fst gram in
  let syntax = total_function start_sym in

  (* Sort of the same process, but we have to keep track of Rules *)
  (* DFS *)
  
  let make_tree_parsed tree = 
  (*https://caml.inria.fr/pub/docs/manual-ocaml/libref/Option.html*)
    match tree with 
      | None -> None
      | Some x ->
        let reverse_tree = (List.rev x) in
        let rec derive_node tree = 
          match tree with 
            | head::tail ->
              let rec derive_node_list nodes tree= 
                match nodes with 
                | [] -> [], tree
                | first_node::rest ->
                  match first_node with
                  | T term ->
                    let left_side = derive_node_list rest tree in
                    (Leaf term::fst left_side), (snd left_side)
                  | N nterm ->
                    let right_side = derive_node tree in
                    let left_side = derive_node_list rest (snd right_side) in
                    (fst right_side::fst left_side), (snd left_side)
              in
              let derived_nodes = derive_node_list (snd head) tail in
              Node (fst head, fst (derive_node_list (snd head) tail)), (snd derived_nodes)
            (* Sort of empty*)
            | [] -> Node (fst (List.hd tree), []), []
        in
        Some (fst (derive_node reverse_tree))
  in
  make_tree_parsed (extra_parser start_sym syntax total_function in_empty [] frag)

