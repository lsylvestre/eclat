(* ./eclat -relax tests/good/specialize/sp7.ecl  -arg="()" *)

let rec foo(f,x) =
  foo(f,42) ;; (* inifine loop *)

let main () =
  let rec h(y) = h(y + 1) in
  let z = 42 * 3 in
  foo(h,z) ;;

