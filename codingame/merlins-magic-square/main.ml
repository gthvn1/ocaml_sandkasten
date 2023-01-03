open Merlin_square

(* 123 => [1; 2; 3] *)
let decompose_int i =
        let rec inner i l =
                if i < 10 then List.cons i l
                          else inner (i / 10) (List.cons (i mod 10) l) in
        inner i []


(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
let () =
        let row1 = input_line stdin in
        let row2 = input_line stdin in
        let row3 = input_line stdin in
        let allbuttonspressed = input_line stdin in
        let buttonlist = decompose_int (int_of_string allbuttonspressed) in
        let m = M.init_merlin row1 row2 row3 in
        (* Write an answer using print_endline *)
        (* To debug: prerr_endline "Debug message"; *)

        print_endline "-----> Display merlin";
        M.print_merlin m;

        print_string "-----> Buttons pressed:";
        print_endline allbuttonspressed;
        print_endline "-----> Display merlin after transformations";
        M.print_merlin (M.apply_transformations buttonlist m);
