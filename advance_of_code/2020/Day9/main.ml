(**********************************************************************
   The goal is to find the first integer in a list that is not the sum
   of two of the previous X number where X is a size that can vary.
 **********************************************************************)

(* DONE:
        - solve [1; 2; 3; 4; 5; 6; 18] 4;; ==> Some(18)
        - We can read integers from file and generate list of ints
   TODO: use the inputs from the file and solve the problem
 *)

(* Return true if n is the sum of two elements of the list 
 * We take the head of the list and we check *)
let rec is_sum_of n =  function
        | [] -> false
        | _::[] -> false
        | x::xs -> if x >= n
                        then is_sum_of n xs
                        else (* we are looking for y where x + y = n *)
                                let y = n - x in
                                if List.mem y xs
                                then true
                                else is_sum_of n xs

(* take the first nth element of a list *)
let rec take_nth l n =
        if List.length l <= n
        then l
        else 
                let remove_last = List.rev(List.tl(List.rev l)) in
                take_nth remove_last n

(* return a tuple that contains a list of nth element + the n+1 element *)
let take_nth_and_one l n =
        if List.length l < n + 1
        then None
        else Some (take_nth l n, List.nth l n)

(* Return the first element that is not the sum of the previous element in 
   the given window size *)
let rec solve l ws =
        match take_nth_and_one l ws with
        | None -> None
        | Some((xs, n)) -> if is_sum_of n xs
                         then solve (List.tl l) ws
                         else Some(n)

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
        let ws = int_of_string (Array.get Sys.argv 1) in
        let filename = Array.get Sys.argv 2 in
        match solve (list_of_int_from_file filename) ws with
        | None -> Printf.printf "The list is ok\n"
        | Some(x) -> Printf.printf "%d is not the sum of two previous integers\n" x

