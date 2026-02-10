(* ./eclat -relax tests/good/specialize/sp9.ecl  -arg="()" -nostdlib *)

let rec foo(f,x) =
  let _ : int<?n> = x in
  let tab = create<?n>() in
  let _ = (length(tab)) in
  foo(f,42) ;; (* inifine loop *)

let main () =
  let rec h(y) = h(y) in
  let z : int<54> = 42 in
  foo(h,z) ;;

