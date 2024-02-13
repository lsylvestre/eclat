open CustomStdlib ;;

let a = 1 ;;

let rec fact acc n = 
  print_int n;
  if n < 2 then acc else fact (acc*n) (n - a) ;;

print_int (fact 1 6);;
