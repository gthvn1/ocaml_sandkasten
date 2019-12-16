let rec sum_double_digits x = 
        if x < 10 then x * x
        else let d = x mod 10 in
                (d * d) + sum_double_digits (x / 10);;
