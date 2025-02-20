open Effect
open Effect.Deep

type _ Effect.t += Xchg : int -> int Effect.t

let comp1 () = perform (Xchg 0) + perform (Xchg 1)

let () =
  let res =
    try_with comp1 ()
      {
        effc =
          (fun (type a) (eff : a t) ->
            match eff with
            | Xchg n ->
                Some (fun (k : (a, _) continuation) -> continue k (n + 1))
            | _ -> None);
      }
  in
  Printf.printf "%d" res
