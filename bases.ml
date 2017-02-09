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

(*
 *  pgcd 34 12 =
 *    34 = 12 * 2 + 10 (x = 34, y = 12, reste = 10)
 *    12 = 10 * 1 + 2  (x = 12, y = 10, reste = 2)
 *    10 = 2 * 5 + 0   (x = 10, y = 2, reste = 0)
 *)
let rec pgcd x y =
    let r = x mod y in
        if r = 0 then y
        else     pgcd y r;;

let rec fibo n = match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibo (n-1) + fibo (n-2);;

(* terminal version of Fibonacci *)
let fibo_t n =
    let rec f n acc1 acc2 = match n with
        | 0 -> acc1
        | 1 -> acc2
        | _ -> f (n-1) acc2 (acc1 + acc2)
    in f n 0 1;;



(* 123456 -> 6 , retourne le nombre de chiffres *)
let rec nb_chiffres x =
    if x / 10 = 0 then 1
    else          1 + nb_chiffres (x/10);;

let nb_chiffres_t x =
    let rec f x acc =
        if x / 10 = 0 then acc
        else          f (x/10) (acc+1)
    in f x 1;;
