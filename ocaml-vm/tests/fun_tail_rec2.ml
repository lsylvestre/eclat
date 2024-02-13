open CustomStdlib ;;


external identity : 'a -> 'a = "caml_identity" ;;

let rec sum acc n = 
  print_int n;
  if n < 1 then acc else sum (acc + n) (n-1) ;;

print_int (sum 0 (identity 1000));;
