(* ./eclat -arg="()" tests/good/lambda-lifting/t5.ecl -nostdlib *)

operator M.add : (int * int) => int  ;;

let x = M.add(42, 1);;

let f (y,()) =
  M.add(y,x) ;;

let main () =
  let g z = z (1,()) (* because the environment is made 
                        explicit as the form of a value,
                        ``()`` must not be confused with
                          the empty environment *)
  in
  g f ;;
