(* ./eclat -arg="()" tests/good/lambda-lifting/t8-poly.ecl -nostdlib *)

let main () =
  let id x = x in
  let f () = (id true,id 0) in
  f() ;;

  (* we have to avoid function [id] to become a parameter of [f],
     because [let main () =
                let f ((),id) = (id true,id 0) in
                let id x = x in
                f((),id)] would be ill typed]
    since id is not bound by a let and instantiated 
    with two different types. *)