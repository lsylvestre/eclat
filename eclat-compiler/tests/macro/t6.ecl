let main () =
  let f = (+) in
  print_int (generate f 0 ~depth:11)
;;
