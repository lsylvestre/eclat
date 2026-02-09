let main () =
  exec
    let rec f (i) = 
	  print_int(i); 
	  print_newline (); 
	  f(i*10)
    in 
    f(1:int<1023>) 
  default ();;