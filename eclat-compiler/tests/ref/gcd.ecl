let gcd(n,m) =
  let a = ref n in
  let b = ref m in
  let rec loop () =
    if !a < !b then (b := !b - !a; loop ()) else
    if !a > !b then (a := !a - !b; loop ()) else !a
  in loop() ;;

let main () =
 print_int (gcd(6,8));;

