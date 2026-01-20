(* ./eclat -relax tests/todo/specialize/sp5.ecl  -arg="()" *)

let rec apply(g,(i,x)) = g(i,x) ;;

let rec twice(foo,u) = foo(foo(u)) ;;


let main () =
  let rec h(y) = y + 1 in
  let z = 42 * 3 in
  apply(twice,(h,z)) ;;

