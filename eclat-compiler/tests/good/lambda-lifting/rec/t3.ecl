(* ./eclat -arg="()" tests/good/lambda-lifting/rec/t3.ecl -relax *)

let main () =
  let rec h x = x + 1 in
  let rec f g =
    g (h 42)
  in
  print_int (f h);
  print_newline () ;;