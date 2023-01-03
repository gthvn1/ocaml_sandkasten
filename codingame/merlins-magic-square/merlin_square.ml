module M = struct
(* merlin has nine buttons:
        1 2 3
        4 5 6
        7 8 9
   A button is '*' of '~' that means it is lit or not
 *)
let asterisk = '*'
let tilde = '~'

type button = Lit | Unlit
type merlin = button list

let is_a_winner = function
        | [Lit; Lit  ; Lit;
           Lit; Unlit; Lit;
           Lit; Lit  ; Lit] -> true
        | _ -> false

let lit_of_char c =
        if c = tilde then Some(Unlit)
        else if c = asterisk then Some(Lit)
        else None

let char_of_lit = function
        Unlit -> tilde
        | Lit -> asterisk

        (* Convert "* * * ~" into [Lit; Lit; Lit; Unlit] *)
let button_of_string s =
        s
        |> String.to_seq
        |> List.of_seq
        |> List.map lit_of_char
        |> List.filter (function Some(_) -> true | None -> false)
        |> List.map (function Some(x) -> x | _ -> assert false)

        (*
         * true indicates that the state of the button (Lit or Unlit) will change,
 * false indicates that it remains in the same state.
 *)
let transform_of_button = function
        1 -> [true ; true ; false; true ; true ; false; false; false; false]
        | 2 -> [true ; true ; true ; false; false; false; false; false; false]
        | 3 -> [false; true ; true ; false; true ; true ; false; false; false]
        | 4 -> [true ; false; false; true ; false; false; true ; false; false]
        | 5 -> [false; true ; false; true ; false; true ; false; true ; false]
        | 6 -> [false; false; true ; false; false; true ; false; false; true ]
        | 7 -> [false; false; false; true ; true ; false; true ; true ; false]
        | 8 -> [false; false; false; false; false; false; true ; true ; true ]
        | 9 -> [false; false; false; false; true ; true ; false; true ; true ]
        | _ -> failwith "there is only 9 buttons"

let transform_merlin t m =
        let trans (x, y) = match y with
        | Lit -> if x then Unlit else Lit
        | Unlit -> if x then Lit else Unlit
in
        List.combine (transform_of_button t) m
        |> List.map trans

        (* 123 => [1; 2; 3] *)
let decompose_int i =
        let rec inner i l =
                if i < 10 then List.cons i l
                          else inner (i / 10) (List.cons (i mod 10) l) in
        inner i []

        (*
         * Apply transformation takes a number that represents the list of button pushed.
 * For example: [8; 6; 2] means push 8, then 6 then 2.
 *)
let rec apply_transformations bl m = match bl with
          [] -> m
        | b::bs -> apply_transformations bs (transform_merlin b m)

        (* you init merlin row by row
         * row1 = "* * *"
 * row2 = "~ ~ ~"
 * row3 = "* ~ *"
 * Res => merlin [Lit; Lit; Lit; Unlit; Unlit; Unlit; Lit; Unlit; Lit]
 *)
let init_merlin row1 row2 row3 =
        button_of_string (String.concat " " [row1; row2; row3])

let print_merlin m =
        let c = List.map char_of_lit m in
        Printf.printf "%c %c %c\n" (List.nth c 0) (List.nth c 1) (List.nth c 2);
        Printf.printf "%c %c %c\n" (List.nth c 3) (List.nth c 4) (List.nth c 5);
        Printf.printf "%c %c %c\n" (List.nth c 6) (List.nth c 7) (List.nth c 8)


end
