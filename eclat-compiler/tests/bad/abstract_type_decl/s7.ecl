type ('a,'b) t<?n,?m> ;;

let main (x:(bool, unit,(bool * bool)) t<?s,42>) = x ;;

(* ==============================
   file tests/bad/abstract_type_decl/s7.ecl, line 3, characters 39-40:
   Error: type t expects 2 type parameters
   ============================== *)