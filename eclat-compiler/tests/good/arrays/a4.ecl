(* ./eclat -relax tests/good/arrays/a4.ecl  -arg="()" *)

let chrono () = 
  reg (fun c -> c + 1) init 0 ;;

let main () =
  let cy = chrono () in
  let a = create<5>() in
  let b = create<5>() in
  let (v,rdy) = exec let (y1,y2) = (set(a,0,42) || set(b,1,43)) in
                     let (x1,x2) = (get(a,0) || get(b,1)) in
                     x1 + x2
                default 0 
  in
  (* taking 2 cycles *)
  if rdy then (assert ((cy mod 3) = 0);
               assert (v = 85)) ;;

(*
ticks||    1    |     2      ||     3      
y1   || lock(a) | release(a) ||            
y2   || lock(b) | release(b) ||           
x1   ||         | lock(a)    || release(a) 
x2   ||         | lock(b)    || release(b) *)