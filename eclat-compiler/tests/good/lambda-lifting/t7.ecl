(* ./eclat -arg="()" tests/good/lambda-lifting/t7.ecl -nostdlib *)

operator Int.add : (int * int) => int  ;;

let bar(k) = Int.add(43,k);;

let x = Int.add(42, 1);;

let f ((y,foo),()) =
  Int.add(foo(y),x) ;;

let main () =
  let g z = z ((1,bar),())
  in
  g f ;;
