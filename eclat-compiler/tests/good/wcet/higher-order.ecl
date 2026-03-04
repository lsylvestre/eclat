(* ./eclat -relax *)

let g(a,b,k)  = [k(a)||k(b)];;

let x = create<4>();;

let main () =
  g(x,x,(fun a -> set(a,0,42)));;

(*  main : (unit -[2]-> unit) | 0 *)

