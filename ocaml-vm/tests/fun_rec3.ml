open CustomStdlib ;;



let rec f x = 
  print_int x;
  if x < 10 then 42 else (1 + f 0) ;;

print_int (f 38);;
