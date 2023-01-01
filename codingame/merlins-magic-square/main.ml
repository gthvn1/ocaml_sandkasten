type merlin = {
        row1: int list;
        row2: int list;
        row3: int list;
}

let int_of_lit = function
          "~" -> 0
        | "*" -> 1
        | _ -> failwith "lit is ~ or *, nothing else"

let lit_of_int = function
          0 -> "~"
        | _ -> "*"

(* transform "* ~ *" -> [1; 0; 1] *)
let row_of_string s =
        let slist = String.split_on_char ' ' s in
        List.map int_of_lit slist

(* transform [1; 0; 1] -> "* ~ *" *)
let string_of_row r =
        String.concat " " (List.map lit_of_int r)

let print_merlin m =
        print_endline (string_of_row m.row1);
        print_endline (string_of_row m.row2);
        print_endline (string_of_row m.row3)

(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)

let () =
        let input1 = input_line stdin in
        let input2 = input_line stdin in
        let input3 = input_line stdin in
        let allbuttonspressed = input_line stdin in
        let m = {
                row1 = row_of_string input1;
                row2 = row_of_string input2;
                row3 = row_of_string input3;
        } in
        (* Write an answer using print_endline *)
        (* To debug: prerr_endline "Debug message"; *)

        print_merlin m;
        prerr_endline "button pressed"; prerr_endline allbuttonspressed;
        print_endline "answer (a single digit)";
