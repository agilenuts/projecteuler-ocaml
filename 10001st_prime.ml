open Base
open Stdio

let strict_multiples limit number =
  let rec aux multiplier acc =
    let multiplied = number * multiplier in
    if multiplied > limit then acc else aux (multiplier + 1) (multiplied :: acc)
  in
  aux 2 []

let non_primes limit =
  let rec aux num acc =
    if num > limit then acc
    else if Set.mem acc num then aux (num + 1) acc
    else
      aux (num + 1)
        (List.fold (strict_multiples limit num) ~init:acc ~f:Set.add)
  in
  aux 2 (Set.empty (module Int))

let primes limit =
  let np = non_primes limit in
  List.rev
    (Set.fold
       (Set.diff
          (List.fold_left
             (List.init (limit - 1) ~f:(fun x -> x + 2))
             ~init:(Set.empty (module Int))
             ~f:Set.add)
          np)
       ~init:[]
       ~f:(fun lst elt -> elt :: lst))

let ith_prime i =
  let p = primes 1_000_000 in
  let rec aux = function
    | [], _ -> raise (Failure "increase limit")
    | x :: _, 0 -> x
    | _ :: t, n -> aux (t, n - 1)
  in
  aux (p, i)

let () = printf "%d\n" (ith_prime 10_000)
