(* ./eclat -arg="()" tests/good/lambda-lifting/t7.ecl -nostdlib *)

operator M.add : (int * int) => int  ;;

let bar(k) = M.add(43,k);;

let x = M.add(42, 1);;

let f ((y,foo),()) =
  M.add(foo(y),x) ;;

let main () =
  let g z = z ((1,bar),())
  in
  g f ;;
