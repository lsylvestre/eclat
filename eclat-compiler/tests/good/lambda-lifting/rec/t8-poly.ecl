(* ./eclat -arg="()" tests/good/lambda-lifting/rec/t8-poly.ecl -nostdlib -relax *)

let rec main () =
  let rec id x = x in
  let rec f () = (id true,id 0) in
  f() ;;

  (* we have to avoid function [id] to become a parameter of [f],
     because [let rec main () =
                let rec f ((),id) = (id true,id 0) in
                let rec id x = x in
                f((),id)] would be ill typed]
    since id is not bound by a let and instantiated 
    with two different types. *)