let sum_of_multiples limit = 
  let rec sum_of_multiples_inner curr acc = 
      let is_divisible num = (num mod 3) = 0 || (num mod 5) = 0
      in
        if 
          curr >= limit then acc 
        else 
          sum_of_multiples_inner (curr + 1) (acc + if is_divisible curr then curr else 0)
  in 
    sum_of_multiples_inner 1 0

let () = 
  print_int (sum_of_multiples 1000);
  print_newline ();
