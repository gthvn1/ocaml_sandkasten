(** DAY 1

    We have a list of int and we want to find two of them where their
    sum is equal to 2020.
 **)

let rec sum_of_two_equal n = function
        [] -> None
        | x::[] -> None
        | x::xs ->
                let lm = List.map (fun a -> a + x) xs in
                let lf = List.filter (fun b -> b = n) lm in
                match lf with
                [] -> sum_of_two_equal n xs
                | y::_ -> Some(x, y-x)

let rec sum_of_three_equal n = function
        [] -> None
        | x::xs -> if x >= (n - 1) then sum_of_three_equal n xs
                   else match sum_of_two_equal (n - x) xs with
                        | None -> sum_of_three_equal n xs
                        | Some((a, b)) -> Some(x, a, b)

(* ===== Boiler plate for reading list of ints from file ===== *)
let read_whole_file filename =
        let ch = open_in filename in
        let s = really_input_string ch (in_channel_length ch) in
        close_in ch;
        s

(* Read a list of int in a file a return them as a list of int *)
let list_of_int_from_file path =
        path 
        |> read_whole_file
        |> String.split_on_char '\n'
        |> List.filter (fun s -> String.length s > 0)
        |> List.map int_of_string     (* got list of int *)

(* ===== MAIN ===== *)
let () =
        let filename = Array.get Sys.argv 1 in
        let l = list_of_int_from_file filename in
        let res1 = sum_of_two_equal 2020 l in
        let res2 = sum_of_three_equal 2020 l in
        match res1, res2 with
        | Some((x, y)), Some((a, b, c)) -> Printf.printf "%d * %d == %d\n%d * %d * %d == %d\n" x  y (x*y) a b c (a*b*c) 
        | Some((x, y)), None -> Printf.printf "%d * %d == %d\n" x  y (x*y)
        | None, Some((a, b, c)) -> Printf.printf "%d * %d * %d == %d\n" a b c (a*b*c) 
        | None, None -> print_endline "not found"

