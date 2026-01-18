(* let main () =
    reg (fun x -> (x,x) init 0;;*)
(*
let f x = x + 1;;
*)

(*
let g x = x ;;
let f x = (g:int) ;;


let main () =
  f(true) ;;

*)

(*
let f x : int<5> = (0 : int<6>) in x;;
*)

(*
let main () =
  reg (fun (x:int<5>) -> (0 : int<6>)) init (0:int<5>);;
*)

(*


let main () = 
  match A 5 with
  | A x -> x
  | B b -> b;;
*)

(*
type t = A of int | B of bool ;;

let x () = A true ;;
*)
(*
let (a : int * bool) = (true, 42);;
let main () = a ;;*)

(*
let static x = 0^10;;

let f z = let u = x in z ;; 

let main () = f();;*)

(*let g(f) =
  exec 0 default f() ;;

let main () = 
  let rec h x = 42 in
  g(h);;
*)

(*
let rec f x = f (x,x);;

let main () = ();;*)

(*
let f = fun x -> fun y -> 
  x + y;;
*)
(*
let main () = 
  let ((x,y,z),a) = (1,2),5) in x ;;
*)

(*
let rec f x = x + f x * 4 ;;
let main () = f 0 ;;*)

type t1 ;;
type t2 ;;

(* let f z = 
  let ((x,y) : t1 * t2) = z in 
  if true then x else y;;
*)

let f ((x,y) : t1 * t2) = 
  if true then x else y;;


let main () = ();; 

(*let rec f x
let main () = ();;*)