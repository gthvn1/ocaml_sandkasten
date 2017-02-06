type 'a liste =
    | ListeVide
    | ListeNonVide of 'a * 'a liste;;

(* Add an element at the head *)
let cons (tete:'a) (queue:'a liste) = ListeNonVide (tete, queue);;

(* l3 = (1, 2, 3) : l3 is a list of int *)
let l3 = cons 1 (cons 2 (cons 3 ListeVide));;

(* hello = ('h', 'e', 'l', 'l', 'o') *)
let hello = cons 'h' (cons 'e' (cons 'l' (cons 'l' (cons 'o' ListeVide))));;

let est_vide = function
    | ListeVide -> true
    | _        -> false ;;

let rec longueur = function
    | ListeVide -> 0
    | ListeNonVide (_, tail) -> 1 + longueur tail ;;

let operation_sur_liste_droite f init =
    let rec aux = function
        | ListeVide                 -> init
        | ListeNonVide (head, tail) -> (f head (aux tail))
    in aux;;

let operation_sur_liste_gauche f init =
    let rec aux acc = function
        | ListeVide                 -> acc
        | ListeNonVide (head, tail) -> aux (f head acc) tail
    in aux init;;
