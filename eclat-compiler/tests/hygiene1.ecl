let main () =
  let f f =
    let f x = f in
    f 1
  in
  print_int (f 42);
  print_string ","

;;

(* to run: $ ./eclat tests/hygiene1.ecl ; make simul

   ~> 42,42,42,42,42,42,42,42,42,42,42,42,42,42... *)