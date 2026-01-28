(* ./eclat -arg="()" tests/good/lambda-lifting/rec/t1.ecl -relax *)

let x = 42 + 1;;
let y = 3 * 3 ;;

let rec f u =
  u + x ;;

let rec g z =
  f (x + y + z) ;;

let main () =
  print_int (g 101);     (* ~> 196 *)
  print_newline () ;;