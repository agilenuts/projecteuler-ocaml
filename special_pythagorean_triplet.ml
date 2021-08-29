open Base
open Stdio

let prd t = match t with a, b, c -> a * b * c

let sq num = num * num

let special_pythagorean_triplet ~sum =
  let rec aux ~i ~j =
    let k = sum - i - j in
    if k < j then aux ~i:(i + 1) ~j:(i + 2)
    else if sq i + sq j = sq k then (i, j, k)
    else aux ~i ~j:(j + 1)
  in
  aux ~i:1 ~j:2

let () = printf "%d\n" (prd (special_pythagorean_triplet ~sum:1000))
