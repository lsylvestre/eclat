(* ./eclat -relax tests/good/arrays/a4.ecl  -arg="()" *)

let chrono () = 
  reg (fun c -> c + 1) init 0 ;;

let main () =
  let cy = chrono () in
  let a = create<5>() in
  let (v,rdy) = exec let (y1,y2) = ((set(a,0,42); get(a,0)) || set(a,0,43)) in
                     y1
                default 0 
  in
  (* taking 3 cycles *)
  if rdy then (assert ((cy mod 4) = 0);
               assert(v = 42)) ;;

(*
ticks||    1    |          2          |     3      ||
y1   || lock(a) | release(a); lock(a) | release(a) ||
y2   || X       | X                   | lock(a)    || release(a) *)