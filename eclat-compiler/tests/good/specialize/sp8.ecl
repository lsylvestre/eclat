(* ./eclat -relax tests/good/specialize/sp8.ecl  -arg="()" -nostdlib *)

let foo(f,x) =
  let _ : int<'N> = x in
  let tab = create<'N>() in
  length(tab) ;;

let main () =
  let rec h(y) = h(y) in
  let z : int<54> = 42 in
  foo(h,z) ;;

