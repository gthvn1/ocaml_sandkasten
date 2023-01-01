(* merlin has nine buttons:
        1 2 3
        4 5 6
        7 8 9
   A button is '*' of '~' that means it is lit or not
 *)
type button = Lit | Unlit
type merlin = button list

let lit_of_char = function
          '~' -> Unlit
        | '*' -> Lit
        | _ -> failwith "lit is ~ or *, nothing else"

let char_of_lit = function
          Unlit -> "~"
        | Lit -> "*"

(* Convert "* * *" into [Lit; Lit; Lit] *)
let button_of_string s =
        s
        |> String.to_seq
        |> List.of_seq
        |> List.filter (fun c -> c != ' ')
        |> List.map lit_of_char

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

let transform_merlin b m =
        let tr (x, y) = match y with
        | Lit -> if x then Unlit else Lit
        | Unlit -> if x then Lit else Unlit
        in
        List.combine (transform_of_button b) m
        |> List.map tr

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
        Printf.printf "%s %s %s\n" (List.nth c 0) (List.nth c 1) (List.nth c 2);
        Printf.printf "%s %s %s\n" (List.nth c 3) (List.nth c 4) (List.nth c 5);
        Printf.printf "%s %s %s\n" (List.nth c 6) (List.nth c 7) (List.nth c 8)

(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
let () =
        let row1 = input_line stdin in
        let row2 = input_line stdin in
        let row3 = input_line stdin in
        let allbuttonspressed = input_line stdin in
        let m = init_merlin row1 row2 row3 in
        (* Write an answer using print_endline *)
        (* To debug: prerr_endline "Debug message"; *)

        print_endline "----------------------";
        print_merlin m;

        print_endline "answer (a single digit)"
