(* Exos from http://lucas.texier.free.fr/OCAML_exercice.html *)

(*****************************************************************************
 * EXO 1
 * Calculer la somme de deux entiers en récursif (Classique et Terminale).
 * On suppose que a et b sont positifs.
 *)
let rec sommeC a = function
  | 0 -> a
  | b -> 1 + sommeC a (b - 1)

let sommeT a b =
    let rec innerSomme acc x = function
      | 0 -> acc + x
      | y -> innerSomme (acc + 1) x (y - 1)
    in
      innerSomme 0 a b

(*****************************************************************************
 * EXO 2
 * Calculer le produit de deux entiers en récursif (Classique et Terminale).
 * On suppose que a et b sont positifs.
 *)
let rec produitC a b = match a with
  | 0 -> 0
  | 1 -> b
  | _ -> b + (produitC (a-1) b)

let produitT a b =
  let rec innerProduit acc a b = match a with
    | 0 -> 0
    | 1 -> b + acc
    | _ -> innerProduit (b + acc) (a - 1) b
  in
    innerProduit 0 a b

(*****************************************************************************
 * EXO 3
 * PGCD a la mode terminale
 *)
let pgcd =
  let rec inner_pgcd a b =
    if (a mod b = 0) then b
    else inner_pgcd b (a mod b)
  in
    inner_pgcd

(*****************************************************************************
 * EXO 4
 * Fibonacci:  fibo(n)=fibo(n-1)+fibo(n-2)
 *)
let rec fibo = function
    | 0 -> 0
    | 1 -> 1
    | n -> fibo (n-2) + fibo (n-1)

(* La version terminale beaucoup plus performante *)
let fiboT n =
  let rec inner_fibo a b = function
    | 0 -> a
    | 1 -> b
    | x -> inner_fibo b (a+b) (x-1)
  in
    inner_fibo 0 1 n
