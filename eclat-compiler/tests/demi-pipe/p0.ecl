let rec f g =
  print_int 42;
  f g;;

let main () = 
  f (fun y -> (y + 1)) ;;
(* let demi_pipe(f) =
  let rec loop(i) =
               let _ = f 0 in
               loop(i+1)
  in loop(0) ;;

let f2 x = x + 2 ;;

let pipe3 (f1) =
  demi_pipe(f1);;

let main () =
    pipe3(f2);; *)
  