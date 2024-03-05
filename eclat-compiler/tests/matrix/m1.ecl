let static m = 42^2^6;;

let main () =
  m.(5).(0) <- 10;
  print_int (m.(5).(0));;