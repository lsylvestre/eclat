
let rec gcd(a,b) =
  if a < b then gcd(a,b-a)
  else if a > b then gcd(a-b,b)
  else a ;;

let main () =
  reg (fun () -> print_string "|") last ();
  let ((),rdy) =
  exec
    let x = gcd(2,1)
    and y = gcd(5,1) in
    print_int (x + y)
  default ()
  in
  rdy ;;

(* to run: $ ./eclat tests/par3.ecl ; make simul

   ~> ||||||2||||||2||||||2|||| *)