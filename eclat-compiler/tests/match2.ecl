type t = A of int | B of int * int | C of bool

let main z =
  (match z with
  | A x -> print_int (x)
  | B (x1,x2) -> print_int (x1+x2)
  | C b -> print_string (if b then "foo" else "bar")
  end);
  print_string "," ;;


(* to run: $ ./eclat tests/match2.ecl \
               -arg "A(2);B(1,2);A(4);C(true);A(2)" \
              ; make simul

   ~> 2,3,4,foo,2,2,2,2,... *)