(* Exercices: https://fr.wikibooks.org/wiki/Objective_Caml/Bases *)

let f x =
    x *. x -. 2. *. x +. 1.;;

let presque_zero epsilon x = (-.epsilon < x) && (x < epsilon);;

let appliquer_operation_postfixe x y o = o x y;;

let preque_racine_generique f epsilon x =
    presque_zero epsilon (f x);;

let presque_racine = preque_racine_generique f;;

(*
 * Return the sum between n and m by adding n, n+1, ... m
 *)
let rec somme_des_nombres =
    fun n ->
        fun m ->
            if n > m then 0
            else     n + (somme_des_nombres (n+1) m);;
