(* ./eclat -relax tests/good/arrays/a3.ecl  -arg="()" *)

let chrono () = 
  reg (fun c -> c + 1) init 0 ;;

let main () =
  let cy = chrono () in
  let a = create<5>() in
  let (v,rdy) = exec let (y1,y2) = (set(a,0,42) || set(a,1,43)) in
                     let (x1,x2) = (get(a,0) || get(a,1)) in
                     x1 + x2
                default 0 
  in
  (* set*2+get*2 = 4 cycles *)
  if rdy then (assert ((cy mod 5) = 0);
               assert (v = 85)) ;;

(*
ticks||    1    |     2      |     3      |    4       ||
y1   || lock(a) | release(a) |            |            ||
y2   ||    X    | lock(a)    | release(a) |            ||
x1   ||         |            | lock(a)    | release(a) ||
x2   ||         |            |    X       | lock(a)    || release(a) *)