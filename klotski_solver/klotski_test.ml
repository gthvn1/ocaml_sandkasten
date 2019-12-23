(* Test loop: find the first number divisble by 5 *)
let%test _ = Klotski.loop (fun x -> if x mod 5 == 0 then true else false)
    (fun x -> x + 1) 2 = 5

let%test _ = Klotski.loop (fun x -> if x mod 5 == 0 then true else false)
    (fun x -> x + 1) 11 = 15

let%test _ = Klotski.loop (fun x -> if x mod 5 == 0 then true else false)
    (fun x -> x + 1) 15 = 15

(* Test exists: return true iif there is an x in list with p x is true *)
let%test _ = Klotski.exists (fun x -> x mod 2 == 0) [1; 3; 5; 7] = false
let%test _ = Klotski.exists (fun x -> x mod 2 == 0) [1; 3; 6; 7] = true

(* Test find: return the element that satisfies p *)
let%test _ = Klotski.find (fun x -> x mod 2 == 0) [1; 3; 6; 7] = 6

(* Test near *)
let%test _ = Klotski.near (-2) = [-4; -3; -2; -1; 0]
let%test _ = Klotski.near 0 = [-2; -1; 0; 1; 2]
let%test _ = Klotski.near 2 = [0; 1; 2; 3; 4]
let%test _ = Klotski.near 3 = [1; 2; 3; 4; 5]
let%test _ = Klotski.near 4 = [2; 3; 4; 5; 6]

(* Test flat_map: applies r to a list. If we apply the relation near to
 * the list [2; 3; 4] we should have the concatenation of the three list
 * given in the exemple above.
 *)
let%test _ =
  Klotski.flat_map Klotski.near [2; 3; 4] = [0;1;2;3;4;1;2;3;4;5;2;3;4;5;6]

(* Test iter_rel: returns a function that applies the relation to the new set
 * of computed relation list and does this n times.
 * Example with relation "near"
 * step 0: near 0 => [0]
 * step 1: near [0] => [-2; -1; 0; 1; 2]
 * step 2: near [-2; -1; 0; 1; 2] = [-4; -3; -2; -1; 0; -3; -2; -1; 0; 1; -2; -1; 0; 1; 2; -1; 0; 1; 2; 3; 0; 1; 2; 3; 4]
 * etc...
 *)
let%test _ =
  let deep0 = Klotski.iter_rel Klotski.near 0 in
  deep0 0 = [0]
let%test _ =
  let deep1 = Klotski.iter_rel Klotski.near 1 in
  deep1 0 = [-2; -1; 0; 1; 2]
let%test _ =
  let deep2 = Klotski.iter_rel Klotski.near 2 in
  deep2 0 = [-4; -3; -2; -1; 0; -3; -2; -1; 0; 1; -2; -1; 0; 1; 2; -1; 0; 1; 2; 3; 0; 1; 2; 3; 4]

(* Test solve:
 *)
let%test _ = Klotski.solve Klotski.near (fun x -> x = 12) 0 = 12

let%test _ = Klotski.solve_path Klotski.near (fun x -> x = 12) 0 = [0; 2; 4; 6; 8; 10; 12]
