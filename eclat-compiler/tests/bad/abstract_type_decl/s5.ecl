type t<?n,?m> ;;

let main (x:bool t<?s,42>) = x ;;

(* ==============================
   file tests/bad/abstract_type_decl/s5.ecl, line 3, characters 17-18:
   Error: type t expects 0 type parameter
   ============================== *)