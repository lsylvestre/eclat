let rec k x = x;;
let rec u x = let z = (k x) in if (z == 1) or (z == 12) then x else u z;;
let rec gcd(a,b) =
  if a < b then gcd(a,b-a)
  else if a > b then gcd(a-b,b)
  else u a ;;

let main () =
  (* let (r,_) = exec (gcd(2,6)) default 2 in r + 4 ;;*)
  let x = gcd(1,10) in gcd(11+x,11+x);;
  (* exec (gcd(2,6) + gcd(3,3)) default 2 ;; *)
