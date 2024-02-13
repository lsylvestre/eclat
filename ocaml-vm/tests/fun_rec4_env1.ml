open CustomStdlib ;;


let a = 1 ;;

let rec fact x = 
  print_int x;
  if x < 2 then a else (x * fact (x-1)) ;;

print_int (fact 6);;
