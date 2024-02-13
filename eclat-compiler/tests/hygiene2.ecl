let main () =
  let f = 42 in
  let f x = f in
  let c = reg (fun f -> f + 1) last 0 in
  print_int (c + f c);
  print_string ","
;;

(* to run: $ ./eclat tests/hygiene2.ecl ; make simul

   ~> 43,44,45,46,47,48,49,50,51,52,53,54,55,... *)