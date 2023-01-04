(* =========================================================== *)
(* ===== Boiler plate for reading list of ints from file ===== *)
let read_whole_file filename =
        let ch = open_in filename in
        let s = really_input_string ch (in_channel_length ch) in
        close_in ch;
        s

(* generate a list of int with -1 if the int is the string is empty *)
let list_of_int_from_file path =
        path 
        |> read_whole_file
        |> String.split_on_char '\n'
        |> List.map (fun s -> if String.length s > 0 then int_of_string s else (-1))

(* This fold function allows to transform
 *   [1000; 2000; -1; 50; 100; 30; -1] 
 * into
 *   [0; 180; 3000]
 *)
let fold_f l x =
        if x <> -1
        then match l with
                | [] -> [x]
                | y::ys -> (y + x)::ys
        else List.append [0] l

let get_most_cal l = l
        |> List.fold_left fold_f []
        |> List.fold_left max 0

(* =========================================================== *)
(* ======================= MAIN ============================== *)

let () =
        let fname = Array.get Sys.argv 1 in
        let l = list_of_int_from_file fname in
        let max_cal = get_most_cal l in
        print_int max_cal
