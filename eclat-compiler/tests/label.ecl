let static l = 42^15 ;;

let foo i = ^ w . print_int (w[i]) ;;

(* let foo i = ^ w . (print_int (w[i])) ;; *)

let main x =
  exec (foo(0))<l> default ()
 ;;
