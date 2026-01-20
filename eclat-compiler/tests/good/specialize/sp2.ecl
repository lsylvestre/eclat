(* ./eclat -relax tests/good/specialize/sp2.ecl  -arg="()" *)

let apply(g,(i,x)) = g(i,x) ;;

let twice(foo,u) = foo(foo(u)) ;;


let main () =
  let h(y) = y + 1 in
  let z = 42 * 3 in
  apply(twice,(h,z)) ;;

