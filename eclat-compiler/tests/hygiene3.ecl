let main () =
  let f = 1 in
  let f x = x + f in
  let f = reg (fun y -> f y) last 0 in
  print_int (f);
  print_string ","

;;

(* to run: $ ./eclat tests/hygiene3.ecl ; make simul

   ~> 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,... *)