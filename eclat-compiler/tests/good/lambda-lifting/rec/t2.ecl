(* ./eclat -arg="()" tests/good/lambda-lifting/rec/t2.ecl -nostdlib -relax *)

operator M.add : (int * int) => int  ;;

let x = M.add(42, 1);;

let rec f u =
  M.add(u,x) ;;

let main () =
  let rec g z = z 43 (* the arity of the functional parameter 
                        change during lambda-lifting *)
  in
  g f ;;
