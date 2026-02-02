type t ;;

let main (x:t<5>) = x ;;

(* ==============================
   file tests/bad/abstract_type_decl/s1.ecl, line 3, characters 12-13:
   Error: type t expects 0 size parameter 
   ============================== *)