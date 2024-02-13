
let rec k x = x;;
let rec u x = let z = (k x) in if (z == 1) or (z == 12) then x else u z;;
let rec gcd(a,b) : int<16> =
  if a < b then gcd(a,b-a)
  else if a > b then gcd(a-b,b)
  else u a ;;


let counter(reset) : int<16> =
  let inc(c) =
    if reset then 0 else c + 1
  in
  reg inc last 0 ;;

let main () =
  let v = counter(false) in
  let w = counter(v = 5) in
  exec
    let n = counter(false) in
    let w = counter(v = 5) in
    let x = gcd(n,10) in
    gcd(11+x,11+x)
  default v ;;
  (* exec (gcd(2,6) + gcd(3,3)) default 2 ;; *)
