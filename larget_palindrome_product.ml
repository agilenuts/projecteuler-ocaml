let digits x = 
  let rec aux y acc = 
    if y = 0 then acc
    else aux (y/10) (acc @ [y mod 10])
  in aux x []

let is_palin x = let d = digits x in d = List.rev d

let largest_palindrome lo hi =
  let rec aux i j r =
    if i > hi then r
    else if j > hi 
    then aux (i+1) lo r
    else if is_palin (i*j)
    then aux i (j+1) (max r (i*j))
    else aux i (j+1) r
in aux lo lo (-1)

let () = 
  print_int (largest_palindrome 100 999);
  print_newline ();
