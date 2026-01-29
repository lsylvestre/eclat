(* ./eclat -arg="()" tests/good/lambda-lifting/t6.ecl -nostdlib *)

operator Int.add : (int * int) => int  ;;

let x = Int.add(42, 1);;

let f (y,()) =
  Int.add(y,x) ;;

let main () =
  let g z = z (1,()) (* because the environment is made 
                        explicit as the form of a value,
                        ``()`` must not be confused with
                          the empty environment *)
  in
  g f ;;
