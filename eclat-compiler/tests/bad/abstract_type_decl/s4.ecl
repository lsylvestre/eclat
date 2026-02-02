type t<?n,?m> ;;

let main (x:t<5,5,5>) = x ;;

(* ==============================
   file tests/bad/abstract_type_decl/s4.ecl, line 3, characters 12-13:
   Error: type t expects 2 size parameters
   ============================== *)