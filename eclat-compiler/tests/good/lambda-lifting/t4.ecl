(* ./eclat -arg="()" tests/good/lambda-lifting/t4.ecl -nostdlib *)

operator M.add : (int * int) => int  ;;

let x = M.add(42, 1);;

let f () =
  M.add(1,x) ;;

let main () =
  let g z = z () (* because the environment is made 
                    explicit as the form of a value,
                    ``()`` must not be confused with
                    the empty environment *)
  in
  g f ;;
