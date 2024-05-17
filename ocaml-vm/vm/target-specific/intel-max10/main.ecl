(* [counter (rst)] is instantaneous,
   it increments its output each time it is re-executed *)
let counter (rst) =
  let inc(c) =
    if rst then 0 else c + 1
  in
  reg inc last 0 ;;


(* blinking a led each n clock cycles *)
let blink (n) =
  let inc c = if c = n + n then 0 else c + 1 in
  (reg inc last 0) > n
;;



(* see in IOs.ecl definitions of types inputs and outputs *)

let main (ii : inputs) : outputs =
  let (_,(key0,key1)) = ii in
  (* suspend the VM when input [key0] is true *)
  if not(key0) then
    (all_switches key1, display_coffee)
  else
  (* reset the counter when input [key1] is true *)
  let cy = counter(key1) in

  let (stop,busy,led) = ocaml_vm (key1) in

  (if stop then (print_string("(cy=");
                 print_int(cy);
                 print_string(")");
                 print_newline ()) else ());

  (* blinking a led *)
  let b = blink(10000000) in
  let switches = (stop,busy,b,led,false,false,false,false,false,false) in
  (switches, display_zero) ;;
