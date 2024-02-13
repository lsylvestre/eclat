type t = A of int | B of int * int | C of bool | D of bool * bool | E of unit

let main z =
  (match z with
  | A x -> print_int (x)
  | B (x1,x2) -> print_int (x1+x2)
  | C b -> print_string (if b then "foo" else "bar")
  | D _ -> print_string "##"
  | E _ -> print_string "??"
  end);
  print_string "," ;;


(* to run: $ ./eclat tests/match4.ecl \
                   -arg "A(2);D(true,false);B(1,2);A(4);D(true,true);C(true);E();A(2)" \
             ; make simul

   ~> 2,##,3,4,##,foo,??,2,2,2 *)