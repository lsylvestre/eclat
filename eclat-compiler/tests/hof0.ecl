let main x =
  let y = 42 in
  let f g = g x + y in
  let twice z = z * 2 in
  print_int (f twice);
  print_string "," ;;


(* to run: $ ./eclat tests/hof0.ecl -arg "10;100;1000" ; make simul

   ~> 62,242,2042,2042,2042,... *)