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
let%test _ = Klotski.near 2 = [0; 1; 2; 3; 4]
let%test _ = Klotski.near 3 = [1; 2; 3; 4; 5]
let%test _ = Klotski.near 4 = [2; 3; 4; 5; 6]

let%test _ = Klotski.near 0 = [-2; -1; 0; 1; 2]
let%test _ = Klotski.near (-2) = [-4; -3; -2; -1; 0]

(* Test flat_map: applies r to a list. If we apply the relation near to
 * the list [2; 3; 4] we should have the concatenation of the three list
 * given in the exemple above.
 *)
let%test _ =
  Klotski.flat_map Klotski.near [2; 3; 4] = [0;1;2;3;4;1;2;3;4;5;2;3;4;5;6]
