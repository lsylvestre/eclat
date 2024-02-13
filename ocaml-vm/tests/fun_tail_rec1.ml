open CustomStdlib ;;


let rec f x = 
  print_int x;
  if x < 2 then 200 else f 0 ;;

print_int (f 42);;
