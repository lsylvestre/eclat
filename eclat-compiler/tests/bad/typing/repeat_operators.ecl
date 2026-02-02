operator M.f : bool * bool => unit ;;
operator M.f : bool * bool => unit ;; 
  (* ==============================
     $ ./eclat tests/bad/typing/repeat_operators.ecl -Werror
     file tests/bad/typing/repeat_operators.ecl, line 2, characters 9-12:
     Error: Redefinition of operator M.f
     Previous declaration at location: file tests/bad/typing/repeat_operators.ecl, line 1, characters 9-12
     ============================== *)

let main () = () ;;