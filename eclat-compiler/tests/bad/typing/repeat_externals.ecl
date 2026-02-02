external f : bool * bool => unit ;;
external f : bool * bool => unit ;; (* should fail (due to the redefinition) *)

let main () = () ;;
  
  (* ==============================
     $ ./eclat tests/bad/typing/repeat_externals.ecl -Werror 
     file tests/bad/typing/repeat_externals.ecl, line 2, characters 9-10:
     Error: Redefinition of external function f
     Previous declaration at location: file tests/bad/typing/repeat_externals.ecl, line 1, characters 9-10
     ============================== *)