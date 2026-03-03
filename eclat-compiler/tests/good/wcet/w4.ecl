(* ./eclat -relax *)

let f x =
  [ set(x,0,42) || set(x,0,43) || set(x,0,44)  ] ;;

(* val f : forall ?v1 :v2 ?v3  . (int<?v1> array<?v3>['v2!] 
     -[max(max(#{'v2!:1},#{'v2!:1}),#{'v2!:1})]-> unit) | 0 
 *)

let main () =
   let x = create<5>() in
   f x ;;

(*
   concurrent accesses to a same array are sequentialized:

   val main : (unit -[3]-> unit) | 0 
*)