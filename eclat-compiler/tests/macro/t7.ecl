let static d = 11 ;;

let sum = /\ ~x .   (* calcule 0 + 1 + ... (x-1) *)
  generate (+) (0) ~depth:x ;;

let id = /\ ~x . x ;;

let main (i:bool) =
  let foo = if i then sum else id in
  print_int (foo ~x:d); print_string "," ;;

(* $ ./eclat tests/macro/t7.ecl -arg "true;true;true;false;false;false;true;true;false" 
   $ make simul
     55,55,55,11,11,11,55,55,11,11,11,11, ...
*)