(* ./eclat -arg="()" tests/good/lambda-lifting/t2.ecl -nostdlib *)

operator M.add : (int * int) => int  ;;

let x = M.add(42, 1);;

let f u =
  M.add(u,x) ;;

let main () =
  let g z = z 43 (* the arity of the functional parameter 
                    change during lambda-lifting *)
  in
  g f ;;
