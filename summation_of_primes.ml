open Base
open Stdio

let composites ~below =
  let multiples ~of_prime =
      (List.init (below / of_prime) ~f:(fun x -> (x + 2) * of_prime))
  in
  let rec aux ~i ~s =
    if i >= below then s
    else if Set.mem s i then aux ~i:(i + 1) ~s
    else aux ~i:(i + 1) ~s:(List.fold (multiples ~of_prime:i) ~init:s ~f:Set.add)
  in
  aux ~i:2 ~s:(Set.empty (module Int))

let primes ~below =
  let all_nums =
    Set.of_list (module Int) (List.init (below - 1) ~f:(fun x -> x + 2))
  in
  Set.to_list (Set.diff all_nums (composites ~below))

let () = printf "%d\n" (List.fold (primes ~below:2_000_000) ~init:0 ~f:( + ))
