let max(x,y) = if x > y then x else y ;;

let collatz(n) =
  let rec loop(n,t) =
    if n = 1 then t else
    if (n mod 2) = 0 then loop(n/2,t+1)
    else loop(n*3+1,t+1)
  in loop(n,1) ;;
  
let main(pressed) = 
  reg (fun (x,i) ->
    if pressed then (x,i) else
    let (next,rdy) = exec (
         let x1 = collatz(i) 
         and x2 = collatz(i+1)
         and x3 = collatz(i+2)
         and x4 = collatz(i+3) in
         let m = max(x,max(x1,x2)) in
         (m,i+2)
      ) default (x,i)
    in next
  ) last (0,1) ;;
(*
type t = Off of unit | On of unit | Done of int

let collatz(n) =
  let rec loop(n,t) =
    if n = 1 then t else
    if (n mod 2) = 0 then loop(n/2,t+1)
    else loop(n*3+1,t+1)
  in loop(n,1) ;;

let main(suspend,pressed,n) =
  reg (fun st ->
    if suspend then st else
    match st with
    | Off() -> if pressed then On() else Off()
    | On() -> let (x,rdy) = exec collatz(n) * 3 default 0 in
            if rdy then Done(x) else On()
    | Done(x) -> Off()
    end) last (Off())  ;;
*)

(* 


*)

(* let collatz(n) =
  let rec loop(n,t) =
    if n <= 1 then t else
    if (n mod 2) = 0 then loop(n/2,t+1)
    else loop(n*3+1,t+1)
  in loop(n,1) ;;

let foo(f,n) =
  let x = f(n) and y = f(n+1) in 
  (x + y) ;;

(* let counter(b) =
  reg (fun c -> 
         if b then c + 1 else c) 
  last 1 ;;
*)
let main (i,suspend) =
  if suspend then (0,false) else 
  (* let c = counter(i) in*)
  exec foo(collatz,i) default 0
;;
*)