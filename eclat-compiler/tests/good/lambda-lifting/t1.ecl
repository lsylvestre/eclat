(* ./eclat -arg="()" tests/good/lambda-lifting/t1.ecl  *)

let x = 42 + 1;;
let y = 3 * 3 ;;

let f u =
  u + x ;;

let g z =
  f (x + y + z) ;;

let main () =
  print_int (g 101);     (* ~> 196 *)
  print_newline () ;;