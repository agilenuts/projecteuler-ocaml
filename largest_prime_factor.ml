let rec repeated_divide_reduce n d = if n mod d = 0 then repeated_divide_reduce (n/d) d else n  

let lpf x = 
  let rec lpf_inner i residue =
    let reduced = repeated_divide_reduce residue i in
      if reduced = 1 then i
      else if i * i > x then x
      else lpf_inner (i+1) reduced
  in lpf_inner 2 x

let () = 
  print_int (lpf 600851475143);
  print_newline ();
