(* A data type combined two kind of types:
 * - product types
 * - variant types (sum type)
 * Product type are related to cartesian product in mathematical set theory.
 * Whereas the variant types are related to disjoint union.
 *)

(* Here is an example of sum type *)
type couleur_de_carte = Trefle | Carreau | Coeur | Pique ;;

type couleur_ou_erreur = Couleur of couleur_de_carte | Erreur ;;

(* So we have a new sum type that is the union of singleton (the constructors)
 * and we can create a color like this *)
let une_couleur:couleur_de_carte = Trefle;; (* Trefle is a constructor of type couleur_de_carte *)

let couleur_to_string c = match c with
    | Trefle  -> "trefle"
    | Carreau -> "carreau"
    | Coeur   -> "coeur"
    | Pique   -> "pique";;

(* We can have a more complex union combined with product type (see the last one) *)
type carte_de_tarot =
    | Excuse
    | Atout of int
    | Roi of couleur_de_carte
    | Dame of couleur_de_carte
    | Cavalier of couleur_de_carte
    | Valet of couleur_de_carte
    | Nombre of int * couleur_de_carte;;

let get_couleur = function
    | Excuse | Atout _                      -> Erreur
    | Roi c | Dame c | Cavalier c | Valet c -> Couleur c
    | Nombre (_,c)                          -> Couleur c;;

(* If we don't force the type to couleur_de_carte the following issue
 * can occurs:
 *     # est_couleur "toto" (Roi Trefle)
 *     # bool = true
 * It is not what we want.
 *)
let est_couleur (couleur:couleur_de_carte) = function
    | Roi      couleur      -> true
    | Dame     couleur      -> true
    | Cavalier couleur      -> true
    | Valet    couleur      -> true
    | Nombre   (_, couleur) -> true
    | _                     -> false;;

let le_petit = Atout 1;;

let string_of_carte_de_tarot = function
    | Excuse          -> "Excuse"
    | Atout    i      -> string_of_int i ^ " d'atout"
    | Roi      c      -> "Roi de " ^ (couleur_to_string c)
    | Dame     c      -> "Dame de " ^ (couleur_to_string c)
    | Cavalier c      -> "Cavalier de " ^ (couleur_to_string c)
    | Valet    c      -> "Valet de " ^ (couleur_to_string c)
    | Nombre   (i, c) -> (string_of_int i) ^ " de " ^ (couleur_to_string c);;

string_of_carte_de_tarot le_petit;;
string_of_carte_de_tarot (Atout 5);;
string_of_carte_de_tarot (Roi Trefle);;
