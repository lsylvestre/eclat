(* ./eclat -arg="()" tests/good/lambda-lifting/rec/t4.ecl -nostdlib -relax *)

operator Int.add : (int * int) => int  ;;

let x = Int.add(42, 1);;

let rec f () =
  Int.add(1,x) ;;

let main () =
  let rec g z = z () (* because the environment is made 
                        explicit as the form of a value,
                        ``()`` must not be confused with
                        the empty environment *)
  in
  g f ;;
