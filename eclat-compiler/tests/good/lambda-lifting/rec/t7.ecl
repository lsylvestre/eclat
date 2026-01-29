(* ./eclat -arg="()" tests/good/lambda-lifting/rec/t7.ecl -nostdlib -relax *)

operator Int.add : (int * int) => int  ;;

let rec bar(k) = Int.add(43,k);;

let x = Int.add(42, 1);;

let rec f ((y,foo),()) =
  Int.add(foo(y),x) ;;

let rec main () =
  let rec g z = z ((1,bar),())
  in
  g f ;;
