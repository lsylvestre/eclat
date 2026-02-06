(* ./eclat -relax -nostdlib ../examples/todo/unbound.ecl *)

let g (f) =
   (* f does not occur in then body *)
   ()
 ;;

let main () =
  let f = (fun (i,_) -> i) in
  g(f) ;;
