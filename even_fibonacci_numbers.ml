let fib_sum limit = 
  let rec fib_sum_inner a b acc =
    let is_even x = x mod 2 = 0 in 
      if a > limit then acc else fib_sum_inner b (a+b) (acc + if is_even a then a else 0)
  in fib_sum_inner 1 2 0

let () = 
  print_int (fib_sum 4_000_000);
  print_newline ();
