let main () =
  let f (f,x) = f(x) in
  let id y = y in
  let b = reg (fun x -> not x) last false in
  (if f (id,b)
   then print_int (f (id,2))
   else f (id,print_int 3));
  print_string ","

;;

(* to run: $ ./eclat tests/hygiene5.ecl ; make simul

   ~> 2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3,2... *)