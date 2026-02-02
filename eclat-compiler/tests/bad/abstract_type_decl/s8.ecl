type ('a,'b) t<?n,?m> ;;

let main (x:(bool, t) t<?s,42>) = x ;;

(* ==============================
   file tests/bad/abstract_type_decl/s8.ecl, line 3, characters 19-20:
   Error: type t expects 2 size parameters
   ============================== *)