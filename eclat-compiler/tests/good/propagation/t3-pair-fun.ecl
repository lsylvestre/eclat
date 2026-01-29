
let main () =
  let ((f,g),x) = ( ( (fun x -> x + 1), 
                      (fun y -> y * 2) ), 
                    (print_string "foo"; 42 +1) ) in
  print_int (f(g(x))) ;;