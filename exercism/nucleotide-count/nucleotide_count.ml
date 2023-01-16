open Base

type nucleotide = A | C | G | T

let empty = Map.empty (module Char)

let nucleotide_from_char = function
  | 'A' -> Some(A)
  | 'C' -> Some(C)
  | 'G' -> Some(G)
  | 'T' -> Some(T)
  | _   -> None

let count_nucleotide (s:string) (c:char) =
  let rec inner acc lc = match lc with
    | [] -> Ok acc
    | x::xs -> match nucleotide_from_char x with
                | Some(_) -> inner (if (Char.compare c x) = 0 then (acc + 1) else acc) xs
                | None -> Error x
  in
  match nucleotide_from_char c with
  | None -> Error c
  | _ -> inner 0 (String.to_list s)

let count_nucleotides s =
  let l = String.to_list s in
  let res =   Map.add_exn empty ~key:'A' ~data:0
            |> Map.add_exn ~key:'C' ~data: 0
            |> Map.add_exn ~key:'G' ~data: 0
            |> Map.add_exn ~key:'T' ~data: 0 in
  let update = function
    | Some(a) -> a + 1
    | None -> 0 in
  let rec inner  m lc = match lc with
    | [] -> Ok (Map.filter ~f:(fun v -> not (phys_equal v 0)) m)
    | x::xs -> match nucleotide_from_char x with
                | Some(_) -> inner (Map.update m x ~f:update) xs
                | None -> Error x
  in
  inner res l
