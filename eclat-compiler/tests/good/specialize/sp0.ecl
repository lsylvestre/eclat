(* ./eclat -relax tests/good/specialize/sp0.ecl  -arg="()" *)

let apply(g,x) = g(x) ;;

let main () = 
  let h(y) = y + 1 in
  let z = 42 * 3 in
  apply(h,z) ;;

