open CustomStdlib ;;

()
let rec odd n = 
  print_int n;
  if n < 1 then false else even(n-1)
and even n = 
  print_int n;
  if n < 1 then true else odd(n-1) ;;

print_int (if even 6 then 42 else 200);;
