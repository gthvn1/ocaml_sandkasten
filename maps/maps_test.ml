open OUnit2
open Maps

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they
    are equivalent set-like lists. That means checking two things. First, they
    must both be {set like}, meaning that they do not contain any duplicates.
    Second, they must contain the same elements though not necesseraly in the
    same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq Stdlib.compare lst1 in
  let uniq2 = List.sort_uniq Stdlib.compare lst2 in
     List.length uniq1 = List.length lst1
  && List.length uniq2 = List.length lst2
  && uniq1 = uniq2

(** [pp_pair] pretty prints a pair of [(a, b)] using [pp1] as the
    pretty print function for [a] and [pp2] as the pretty print
    function for [b] *)
let pp_pair pp1 pp2 (a, b) = "(" ^ pp1 a ^ ", " ^ pp2 b ^ ")"

(** [pp_list pp_elmt lst] pretty print a [lst] of pairs where the first element
    of the pair is an int and the second element is a string **)
let pp_list lst =
  let pp_string s = "\"" ^ s ^ "\"" in
  let pp_elt = pp_pair string_of_int pp_string in
  let pp_elts lst =
    let rec loop acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h (* don't print ; for the last elt *)
      | h1 :: t -> loop (acc ^ pp_elt h1 ^ "; ") t
    in
    loop "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let binding_test name input output =
  name >:: fun _ ->
  assert_equal input
    (AssocListMap.bindings output)
    ~printer:pp_list ~cmp:cmp_set_like_lists

let l1 = [ (3110, "fun") ]
let l2 = [ (3110, "fun"); (2110, "oo") ]

let assoc_tests =
  let open AssocListMap in
  [
    binding_test "empty has no bindings" [] empty;
    binding_test "singleton has 1 binding" l1 (of_list l1);
    binding_test "list with 2 bindings" l2 (of_list l2);
    binding_test "adding 1 binding" l1 (empty |> insert 3110 "fun");
    binding_test "adding 2 bindings with same key" l1
      (empty |> insert 3110 "nop" |> insert 3110 "fun");
  ]

let suite = "map suite" >::: assoc_tests
let _ = run_test_tt_main suite
