(* ./eclat -relax tests/good/specialize/sp8.ecl  -arg="()" -nostdlib *)

let foo(f,x) =
  let _ : int<?n> = x in
  let tab = create<?n>() in
  length(tab) ;;

let main () =
  let rec h(y) = h(y) in
  let z : int<54> = 42 in
  foo(h,z) ;;

