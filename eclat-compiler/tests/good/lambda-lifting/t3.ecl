(* ./eclat -arg="()" tests/good/lambda-lifting/t3.ecl  *)

let main () =
  let h x = x + 1 in
  let f g =
    g (h 42)
  in
  print_int (f h);
  print_newline () ;;