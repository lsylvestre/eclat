(* ./eclat -Werror *)

let f (k) = 
    [k(create<4>()) || k(create<4>()) ] ;;

(** Error: loss of sharing in WCET analysis;
    this may cause an underestimation of the execution time **)


let g () = f (fun a -> set(a,0,42));;

let main () = () ;;