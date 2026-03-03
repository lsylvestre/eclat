(* ./eclat -relax *)

let f (x,y,z) =
  [ set(x,0,42) || set(y,0,43) || set(z,0,44)  ] ;;


let main () =
   let x = create<5>() in
   let y = create<5>() in
   f (x,y,x) ;;

(*
   concurrent accesses to a same array are sequentialized:

   val main : (unit -[2]-> unit) | 0 
*)