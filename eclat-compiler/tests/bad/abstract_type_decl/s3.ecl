type t ;;

let main (x:(bool, unit, t) t) = x ;;

(* ==============================
   file tests/bad/abstract_type_decl/s3.ecl, line 3, characters 28-29:
   Error: type t expects 0 type parameter
   ============================== *)