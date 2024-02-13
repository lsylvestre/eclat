open CustomStdlib ;;


let rec f n = 
  print_int n;
  1+ g(n)
and g n = 
  n ;;

print_int (g 123);;
