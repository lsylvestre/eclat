type 'a t<?n,?m> ;;

let main (x:(bool, unit) t<?s,42>) = x ;;

(* ==============================
   file tests/bad/abstract_type_decl/s6.ecl, line 3, characters 25-26:
   Error: type t expects 1 type parameter
   ============================== *)