let rec pause ((),b,r1,r2) = 
  !(if b then r1 else r2) ;;

let main () =
  let r = if true then ref (2 + 3) else ref 6 in
  print_int (pause(print_int 42,(!r = 5),ref 0,ref 1)) ;;


(*
let rec pause (b,r1,r2) =
  !(if b then r1 else r2) ;;

let main () =
  let r1 = ref 0 in
  let r2 = ref 1 in
  let r = ref (2 + 3) in
  print_int (pause((!r = 5),r1,r2)) ;;
*)