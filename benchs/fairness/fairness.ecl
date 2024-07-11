(* Fig. 8 *)

let main () =
  let a = create 3 in
  let rec f (i) = 
    print_int i; print_string ";";
    set(a,i,i); 
    f(i) 
  in
  let ((x,y),z) = ( (f(0) || f(1)) || f(2) ) in () ;;
