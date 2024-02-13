let main () =
  let c = reg (fun (c,d) -> (d,c)) last (3,4) in
  let f (f,g) = f in
  print_int (f c);
  print_string "," ;;


(* to run: $ ./eclat tests/hygiene4.ecl ; make simul

   ~> 4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3... *)