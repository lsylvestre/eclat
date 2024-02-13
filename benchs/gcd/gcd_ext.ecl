
let rec gcd (a,b) =
  if a < b then gcd (a,b-a) else
  if a > b then gcd (a-b,b)
  else a ;;

let rec gcd_ext ((v1,(v2,_)),st) =
  let v = val_long(gcd(long_val(v1),long_val(v2))) in
  (v, st) ;;
