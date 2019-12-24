(*
 * dune build
 *)

let prog1 = [| 1; 0; 0; 3; 99 |];;
let prog2 = [| 1; 3; 0; 3; 99 |];;
let prog3 = [| 1; 0; 0; 3; 1; 1; 2; 3; 99 |];;
let prog4 = [| 1;1;1;4;99;5;6;0;99 |];;
let prog5 =
  [| 1;0;0;3; 1;1;2;3; 1;3;4;3; 1;5;0;3; 2;1;10;19; 2;9;19;23;
     2;23;10;27; 1;6;27;31; 1;31;6;35; 2;35;10;39; 1;39;5;43;
     2;6;43;47; 2;47;10;51; 1;51;6;55; 1;55;6;59; 1;9;59;63;
     1;63;9;67; 1;67;6;71; 2;71;13;75; 1;75;5;79; 1;79;9;83;
     2;6;83;87; 1;87;5;91; 2;6;91;95; 1;95;9;99; 2;6;99;103;
     1;5;103;107; 1;6;107;111; 1;111;10;115; 2;115;13;119;
     1;119;6;123; 1;123;2;127; 1;127;5;0; 99; 2;14;0;0 |]

exception Exit

let () =
  let executed_code = Day2.execute prog5 12 2 in
  print_string "Day1 > fuel needed ";
  print_endline (Day1.total_fuel "input_day1.data");
  print_string "     > real fuel needed ";
  print_endline (Day1.real_total_fuel "input_day1.data");
  print_string "Day2 > ";
  print_int executed_code.(0);
  print_newline ();
  print_string "Day3 > ";
  try
    for i = 0 to 99 do
      for j = 0 to 99 do
        let e = Day2.execute prog5 i j in
        if e.(0) = 19690720 then begin
          print_string "noun = "; print_int i;
          print_string " verb = "; print_int j;
          raise Exit
        end
      done
    done
  with Exit -> print_newline();
