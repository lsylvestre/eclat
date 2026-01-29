(* ./eclat -relax tests/good/arrays/a2.ecl  -arg="()" *)

let chrono () = 
  reg (fun c -> c + 1) init 0 ;;

let main () =
  let cy = chrono () in
  let a = create<5>() in
  let (v,rdy) = exec set(a,0,42);
                     get(a,0) 
                default 0 
  in
  (* get+set = 2 cycles *)
  if rdy then (assert ((cy mod 3) = 0);
               assert (v = 42)) ;;