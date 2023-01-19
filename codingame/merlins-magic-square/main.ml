(**
 * merlin has nine buttons:
 *       1 2 3
 *       4 5 6
 *       7 8 9
 * A button is '*' of '~' that means it is lit or not.
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
        | Unlit -> tilde
        | Lit -> asterisk

        (* Convert "* * * ~ ..." into [Lit; Lit; Lit; Unlit; ...] *)
let button_of_string s =
        s
        |> String.to_seq
        |> List.of_seq
        |> List.map lit_of_char
        |> List.filter (function Some(_) -> true | None -> false)
        |> List.map (function Some(x) -> x | _ -> assert false)

(**
 * "true" indicates that the state of the button (Lit or Unlit) will change,
 * "false" indicates that it remains in the same state.
 *)
let transform_of_button = function
        | '1' -> [true ; true ; false; true ; true ; false; false; false; false]
        | '2' -> [true ; true ; true ; false; false; false; false; false; false]
        | '3' -> [false; true ; true ; false; true ; true ; false; false; false]
        | '4' -> [true ; false; false; true ; false; false; true ; false; false]
        | '5' -> [false; true ; false; true ; true ; true ; false; true ; false]
        | '6' -> [false; false; true ; false; false; true ; false; false; true ]
        | '7' -> [false; false; false; true ; true ; false; true ; true ; false]
        | '8' -> [false; false; false; false; false; false; true ; true ; true ]
        | '9' -> [false; false; false; false; true ; true ; false; true ; true ]
        | _ -> failwith "there is only 9 buttons"

(**
 * Take a transformation (t) that is related to the button's value
 * and apply it to merlin (m)
 *)
let transform_merlin butVal m =
        let trans (switch, y) = match y with
        | Lit -> if switch then Unlit else Lit
        | Unlit -> if switch then Lit else Unlit
in
        List.combine (transform_of_button butVal) m
        |> List.map trans

(*
 * Apply transformation takes a number that represents the list of button pushed.
 * For example: ['8'; '6'; '2'] means push 8, then 6 then 2.
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

let merlin_to_string m =
        let mc = List.map char_of_lit m in
        let add_newline l =
                let rec inner i = function
                | [] -> []
                | x::xs -> if (i mod 3 = 0)
                           then '\n' :: x :: (inner (i + 1) xs)
                           else x :: (inner (i + 1) xs)
                in
                inner 0 l in
        String.of_seq (List.to_seq (add_newline mc))

(* Find the button to push to get the solution *)
let solve_merlin m =
        let rec find_solution = function
                | [] -> '0'  (* it is impossible in fact *)
                | c::cs -> if (is_a_winner (apply_transformations [c] m))
                                then c
                                else find_solution cs
        in
        find_solution ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']


(* "123" => ['1'; '2'; '3'] *)
let buttons_from_string str =
        List.of_seq (String.to_seq str)


(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
let () =
        let row1 = input_line stdin in
        let row2 = input_line stdin in
        let row3 = input_line stdin in
        let allbuttonspressed = input_line stdin in
        let buttonlist = buttons_from_string allbuttonspressed in
        let m = init_merlin row1 row2 row3 in
        let after_trans = apply_transformations buttonlist m in
        (* Write an answer using print_endline *)
        (* To debug: prerr_endline "Debug message"; *)

        prerr_string "-----> Display merlin";
        prerr_endline (merlin_to_string m);

        prerr_string "-----> Buttons pressed: ";
        prerr_endline allbuttonspressed;
        prerr_string "-----> Display merlin after transformations";
        prerr_endline (merlin_to_string after_trans);

        print_char (solve_merlin after_trans);

