

let main () =
  let rec gcd(a,b) =
  if a < b then gcd(a,b-a)
  else if a > b then gcd(a-b,b)
  else a in
  let x = gcd(1,1) and y = gcd(2,1) in
  x + y ;;