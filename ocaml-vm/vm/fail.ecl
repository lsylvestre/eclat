let fatal_error msg =
  (* fatal error (implemented as an infinite loop)
     in case of invalid_arg error.

     note: cannot be catched by the OCaml program *)
 
  print_string "fatal error: ";
  print_string msg;
  print_newline ();

  let rec forever () = 
    forever ()
  in 
  forever () ;;
