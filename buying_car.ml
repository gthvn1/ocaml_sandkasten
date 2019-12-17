(*
 * A man wants to buy a new car. He thinks he can save $1000 each month but the
 * prices of his old car and of the new one decrease of 1.5 percent per month.
 * Furthermore this percent of loss increases of 0.5 percent at the end of every
 * two months. Our man finds it difficult to make all these calculations.
 *
 * For example:
 * == Start==
 *   - startPriceOld: 2000.0
 *   - startPriceNew: 8000.0
 *   - savingPerMonth: 0.0
 *   - percentageLossByMonth: 1.5
 *
 * == First iteration ==
 *   - startPriceOld: 1970.0
 *   - startPriceNew: 7880.0
 *   - savingPerMonth: 1000.0
 *   - percentageLossByMonth: 2.0
 * => Needs to get 4910
 *
 * == Second iteration ==
 *   - startPriceOld: 1930.60
 *   - startPriceNew: 7722.40
 *   - savingPerMonth: 2000.0
 *   - percentageLossByMonth: 2.0
 * => Needs to get 3791.80
 *
 * etc...
 *)
let rec nb_months (startPriceOld: float) (startPriceNew: float) (savingperMonth: float) (percentLossByMonth: float) =
  let rec aux nbMonths priceOld priceNew perMonth lost =
    let needsToPay = (priceOld +. perMonth) -. priceNew in
    let newlost = if nbMonths mod 2 = 1 then lost +. 0.5 else lost in
    let lostPercent = (100. -. newlost) /. 100. in
    if needsToPay >= 0.0 then (nbMonths, needsToPay)
    else aux (nbMonths + 1) (priceOld *. lostPercent) (priceNew *. lostPercent) (perMonth +. savingperMonth) newlost in
  aux 0 startPriceOld startPriceNew 0.0 percentLossByMonth ;;

(*
module Tests = struct
    open OUnit
    open Printf

    let testing(startPriceOld: float) (startPriceNew: float) (savingperMonth: float) (percentLossByMonth: float) (expectedOutput: int * int) =
      let act = nb_months startPriceOld startPriceNew savingperMonth percentLossByMonth in
        print_string "input "; print_float startPriceOld; print_string "\n";
        print_float startPriceNew;
        print_string "\n";
        print_float savingperMonth;
        print_string "\n";
        print_float percentLossByMonth;
        print_string "\n";
        print_string "Expected "; print_int (fst expectedOutput); print_string(" "); print_int (snd expectedOutput); print_endline "\n got ";
        print_int (fst act); print_string(" "); print_int (snd act); print_endline "\n-----"; print_endline "";
        assert_equal expectedOutput act;;

    let suite = [
        "nb_months" >:::
            [
              "Basic tests" >:: (fun _ ->
                  testing 2000.0 8000.0 1000.0 1.5 (6,766);
                  testing 12000.0 8000.0 1000.0 1.5 (0,4000);
                  testing 18000.0 32000.0 1500.0 1.25 (8,332);
                  testing 7500.0 32000.0 300.0 1.55 (25,122);

                );

            ]
        ]
    ;;
end
*)
