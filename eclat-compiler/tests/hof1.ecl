let main x =
  let y = 42 in
  let f g = g x + y in
  let g = 2 in
  let g z =
    g * x (* <-- here x=2 *)
  in
  print_int (f g);
  print_string "," ;;


(* to run: $ ./eclat tests/hof1.ecl -arg "10;100;1000" ; make simul

   ~> 62,242,2042,2042,... *)