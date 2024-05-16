
(* ./eclat ../examples/reset.ecl -arg="(-7,false);(-1000,true);(6,false)" *)

let fact (n : int<32>) =
  let rec f((acc,n) : (int<32> * int<32>)) =
    if n = 1 then (acc) else f(acc*n,n-1) in f(1,n) ;;

let main(i,b) =
  exec 
    print_int (fact(i)); 
    print_newline () 
  default () reset when b ;;