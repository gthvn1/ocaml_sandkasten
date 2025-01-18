module type T = sig
  type 'a t

  val create : unit -> 'a t
  val get : 'a t -> key:string -> 'a option
  val set : 'a t -> key:string -> value:'a -> unit
  val iter : 'a t -> fn:(string -> 'a -> unit) -> unit
end
