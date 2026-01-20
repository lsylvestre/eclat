(* ./eclat -relax tests/good/specialize/sp1.ecl  -arg="()" *)

let rec apply(g,x) = g(x) ;;

let main () = 
  let h(y) = y + 1 in
  let z = 42 * 3 in
  apply(h,z) ;;

