open CustomStdlib ;;



let rec sum x = 
  print_int x;
  if x < 1 then 0 else (x + sum (x-1)) ;;

print_int (sum 6);;
