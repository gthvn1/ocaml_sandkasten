type 'a t  =
  | Empty
  | Entry of 'a * 'a t

let empty = Empty

let push x s = Entry (x, s)

let peek = function
  | Empty -> failwith "stack is empty"
  | Entry (e, _) -> e

let pop = function
  | Empty -> failwith "stack is empty"
  | Entry (_, s) -> s

