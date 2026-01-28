(* ./eclat -arg="()" tests/good/lambda-lifting/t7.ecl -nostdlib -relax *)

operator M.add : (int * int) => int  ;;

let rec bar(k) = M.add(43,k);;

let x = M.add(42, 1);;

let rec f ((y,foo),()) =
  M.add(foo(y),x) ;;

let rec main () =
  let rec g z = z ((1,bar),())
  in
  g f ;;
