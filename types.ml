(* A data type combined two kind of types:
 * - product types
 * - variant types (sum type)
 * Product type are related to cartesian product in mathematical set theory.
 * Whereas the variant types are related to disjoint union.
*)

(* Here is an example of sum type *)
type couleur_de_carte = Trefle | Carreau | Coeur | Pique

(* So we have a new sum type that is the union of singleton (the constructors)
 * and we can create a color like this *)
let une_couleur:couleur_de_carte = Trefle (* Trefle is a constructor of type couleur_de_carte *)

let couleur_to_string c = match c with
  | Trefle  -> "trefle"
  | Carreau -> "carreau"
  | Coeur   -> "coeur"
  | Pique   -> "pique"

(* We can have a more complex union combined with product type (see the last one) *)
type carte_de_tarot =
  | Excuse
  | Atout of int
  | Roi of couleur_de_carte
  | Dame of couleur_de_carte
  | Cavalier of couleur_de_carte
  | Valet of couleur_de_carte
  | Nombre of int * couleur_de_carte;;

type couleur_ou_erreur =
  | Couleur of couleur_de_carte
  | Erreur

let get_couleur = function
  | Roi c | Dame c | Cavalier c | Valet c -> Some c
  | Nombre (_,c)                          -> Some c
  | _                                     -> None

(* If we don't force the type to couleur_de_carte the following issue
 * can occurs:
 *     # est_couleur "toto" (Roi Trefle)
 *     # bool = true
 * It is not what we want.
 *)
let est_couleur couleur carte = match get_couleur carte with
  | Some couleur      -> true
  | None              -> false

let le_petit = Atout 1

let string_of_carte_de_tarot = function
  | Excuse          -> "Excuse"
  | Atout    i      -> string_of_int i ^ " d'atout"
  | Roi      c      -> "Roi de " ^ (couleur_to_string c)
  | Dame     c      -> "Dame de " ^ (couleur_to_string c)
  | Cavalier c      -> "Cavalier de " ^ (couleur_to_string c)
  | Valet    c      -> "Valet de " ^ (couleur_to_string c)
  | Nombre   (i, c) -> (string_of_int i) ^ " de " ^ (couleur_to_string c)

(* On peut utiliser la fonction de la façon suivante:
    string_of_carte_de_tarot le_petit
    string_of_carte_de_tarot (Atout 5)
    string_of_carte_de_tarot (Roi Trefle)
*)
type ('a, 'b) succes_ou_erreur =
    | Succes of 'a
    | Erreur of 'b

let division x y =
  if y = 0 then Erreur "Division par zero"
  else          Succes (x/y)

(* Let's play with boolean logique *)

type formule =
  | Booleen  of bool
  | Variable of string
  | Et       of formule * formule
  | Ou       of formule * formule
  | Non      of formule
  | Implique of formule * formule

(*
 * formule XOR
 *
 * Rappel:  a | b | xor
 *          -----------
 *          0 | 0 | 0
 *          0 | 1 | 1
 *          1 | 0 | 1
 *          1 | 1 | 0
 *
 * Et donc on peut l'exprimer en disant: (a ou b) et (non (a et b))
 *)

let formule_xor a b = Et (Ou (a, b), Non (Et (a, b)))

let rec evalue arbre = match arbre with
  | Booleen _  -> arbre
  | Variable _ -> arbre
  | Et (f, g) -> (
    match (evalue f, evalue g) with
      | (Booleen false, _            ) -> Booleen false
      | (Booleen _    , Booleen false) -> Booleen false
      | (Booleen true , x            ) -> x
      | (x            , Booleen true ) -> x
      | (x            , Non y        ) -> if x = y then Booleen false
          else          Et (x, y)
      | (x            , y            ) -> Et (x, y))
  | Ou (f, g) -> (
    match (evalue f, evalue g) with
      | (Booleen false, x            ) -> x
      | (x            , Booleen false) -> x
      | (Booleen true , _            ) -> Booleen true
      | (_            , Booleen true ) -> Booleen true
      | (x            , Non y        ) -> if x = y then Booleen true
          else          Ou (x, y)
      | (x            , y)             -> Ou (x, y))
  | Non f -> (
    match (evalue f) with
      | Booleen false -> Booleen true
      | Booleen true  -> Booleen false
      | Non x         -> x
      | x             -> Non x)
  | Implique (f, g) -> (
    match (evalue f, evalue g) with
      | (_            , Booleen true ) -> Booleen true
      | (Booleen false, _            ) -> Booleen true
      | (Booleen true , Booleen false) -> Booleen false
      | (x, y)                         -> Implique (x, y))

(*
 * Si on remplace "Booleen valeur" par valeur la fonction sera alors de
 * type: bytes -> formule -> formule -> formule au lieu de
 *       bytes -> bool -> formule -> formule
 * Elle acceptera donc n'importe quelle formule et non simplement un booleen.
 *)
let rec substitue nom valeur arbre =
  match arbre with
    | Variable v      ->
      if v = nom then Booleen valeur
      else            Variable v
    | Booleen _       -> arbre
    | Et (f, g)       -> Et (substitue nom valeur f, substitue nom valeur g)
    | Ou (f, g)       -> Ou (substitue nom valeur f, substitue nom valeur g)
    | Non f           -> Non (substitue nom valeur f)
    | Implique (f, g) -> Implique (substitue nom valeur f, substitue nom valeur g)

(* A record type - like a tuple with named fields *)
type point2d = {
  x: float;
  y: float;
}

(* On peut définir un point *)
let pt1 = {
  x = 3.;
  y = 4.;
}

(* Et le calcul de la magnitude *)
type circle_desc = {
    center: point2d;
    radius: float;
}

type rect_desc = {
    lower_left: point2d;
    height: float;
    weight: float;
}
