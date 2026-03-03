(* ./eclat -relax *)

let main () =
  let x = create<45>() in
  [ set(x,0,42) || set(x,0,43) || set(x,2,43) ];;

(*  concurrent accesses to a same array are sequentialized:

 
      val main : (unit -[3]-> unit) 
*)