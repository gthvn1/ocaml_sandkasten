(* Exercices: https://fr.wikibooks.org/wiki/Objective_Caml/Bases *)

let f x =
    x *. x -. 2. *. x +. 1.;;

let presque_zero epsilon =
    fun x -> (-.epsilon < x) && (x < epsilon);;

let presque_racine x =
    fun epsilon -> presque_zero epsilon (f x);;
