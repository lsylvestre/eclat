type t = A of int | B of int * int | C of int

let main () =
  (match A 5 with
  | A x -> print_int (x)
  | B (x1,x2) -> print_int (x1+x2)
  | C u -> ()
  end);
  print_string "," ;;


(* to run: $ ./eclat tests/match1.ecl ; make simul

   ~> 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5... *)