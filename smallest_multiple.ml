let exp limit prime = 
  let rec aux acc =
    if acc > limit then (acc/prime)
    else aux (acc*prime)
  in aux 1

let is_prime x = 
  let possible_divisors = List.init (x-2) (fun y -> y+2) in 
    let found = List.find_opt (fun possible_divisor -> x mod possible_divisor = 0) possible_divisors in
      match found with
      | Some _ -> false
      | None -> true

let smallest_multiple limit =
  let nums = List.init (limit-1) (fun y -> y+2) in
    let primes = List.filter is_prime nums in
      let exps = List.map (exp limit) primes in
        List.fold_left ( * ) 1 exps

let () = 
  print_int (smallest_multiple 20);
  print_newline ();
