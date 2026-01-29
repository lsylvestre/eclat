(* ./eclat -relax tests/good/arrays/a1.ecl  -arg="()" *)

let main () =
  let a = create<5>() in
  assert(length(a) = 5) ;;