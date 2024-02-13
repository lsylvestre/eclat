open CustomStdlib ;;


let a = 1 ;;
let b = a+a ;;


let rec fact x = 
  print_int x;
  if x < 2 then a else (x * fact (x+1-b)) ;;

print_int (fact 6);;
