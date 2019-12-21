(* Test loop: find the first number divisble by 5 *)
let%test _ = Klotski.loop (fun x -> if x mod 5 == 0 then true else false)
    (fun x -> x + 1) 2 = 5

let%test _ = Klotski.loop (fun x -> if x mod 5 == 0 then true else false)
    (fun x -> x + 1) 11 = 15

let%test _ = Klotski.loop (fun x -> if x mod 5 == 0 then true else false)
    (fun x -> x + 1) 15 = 15

(* Test near *)
let%test _ = Klotski.near 2 = [0; 1; 2; 3; 4]
let%test _ = Klotski.near 0 = [-2; -1; 0; 1; 2]
let%test _ = Klotski.near (-2) = [-4; -3; -2; -1; 0]
