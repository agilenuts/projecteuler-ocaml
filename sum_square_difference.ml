let square x = x * x

let diff n = let nums = List.init n (fun x -> x+1) in
   square (List.fold_left (+) 0 nums) - (List.fold_left (+) 0 (List.map square nums))

let () = 
  print_int (diff 100);
  print_newline ();
