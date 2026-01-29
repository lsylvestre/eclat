(* ./eclat -arg="()" tests/good/lambda-lifting/rec/t6.ecl -nostdlib -relax *)

operator Int.add : (int * int) => int  ;;

let rec x = Int.add(42, 1);;

let rec f (y,()) =
  Int.add(y,x) ;;

let rec main () =
  let rec g z = z (1,()) (* because the environment is made 
                            explicit as the form of a value,
                           ``()`` must not be confused with
                            the empty environment *)
  in
  g f ;;
