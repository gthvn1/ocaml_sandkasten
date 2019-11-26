(* Signature of the module Dico *)
module type DicoSig = sig
  type ('k, 'v) t
  val empty : ('k, 'v) t
  val add : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t
  val lookup: ('k, 'v) t -> 'k -> 'v
  exception NotFound
end ;;

(*
   Our client module will use a dico as parameter
   it is a functor.
   We just need the siggnature to be able to compile
   this module.
*)
module ForceArchive (Dict: DicoSig) = struct
  let force = Dict.empty
  let force = Dict.add force "luke" 10
  let force = Dict.add force "darth" 20
  let force = Dict.add force "yoda" 50
  let force_of_luke = Dict.lookup force "luke"
  let force_of_d2r2 = Dict.lookup force "d2r2"
end ;;

(* First implementation with list *)
module DicoList : DicoSig = struct
  type ('k, 'v) t = ('k * 'v) list

  exception NotFound

  let empty = []
  let add d k v = (k, v) :: d
  let rec lookup d k = match d with
    | [] -> raise NotFound
    | (k', v')::d' when k = k' -> v'
    | _::d' -> lookup d' k
end ;;

(* Second implementation with binary tree *)
module DicoTree : DicoSig = struct
  type ('k, 'v) t =
    | Empty
    | Node of ('k, 'v) t * 'k * 'v * ('k, 'v) t

  exception NotFound

  let empty = Empty

  let rec add d k v = match d with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (l, k', v', r) ->
      if k = k' then Node (l, k', v', r)
      else if k < k' then Node (add l k v, k', v', r)
      else Node (l, k', v', add r k v)

  let rec lookup d k = match d with
    | Empty -> raise NotFound
    | Node (l, k', v', r) ->
      if k = k' then v'
      else if k < k' then lookup l k
      else lookup r k

end;;

(*
 * Note that as the lookup of d2r2 will raise an exception when evaluating
 * the ForceArchive module, Client1 and Client2 won't be binded. Remove the
 * lookup of d2r2 to be able to do things like: Client1.force ;;
 *)
module Client1 = ForceArchive(DicoList)
module Client2 = ForceArchive(DicoTree)
