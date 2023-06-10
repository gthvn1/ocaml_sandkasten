type 'a t
(** ['a t] is the representation type for ts *)

val empty: 'a t
(** [empty] is the empty t *)

val push: 'a -> 'a t -> 'a t
(** [push x s] is [s] with [x] pushed on top *)

val peek: 'a t -> 'a
(** [peek s] is the top element of [s].
   Raises [Failure] if [s] is empty. *)

val pop: 'a t -> 'a t
(** [pop s] is all but the top element of [s].
   Raises [Failure] is [s] is empty. *)
