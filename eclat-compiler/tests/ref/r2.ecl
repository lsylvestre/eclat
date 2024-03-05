(* let rec foo _ =
  let x = ref 4 in
  foo x
;;

let main () =
  foo (ref 2) ;;
*)

let rec p x = x;;
let v = ((fun x -> x), p 6) ;;

let apply (f,x) = f x ;;

let main () =
  apply((+),(1,2)) ;;

(*
let z = let i = 6 + 6 in ((fun y -> y + i), let v = ref 2 in v) ;;
let v = let u = ((z,z),z) in let (x,_) = u in x ;;
let main () =
  let ((f,_) ,_) = v in f 4 ;;*)

(*
let main () =
 let x = ref 0 in
 let y = ref true in
  let f(r,v) =
    r := v
  in 
  f(x,42);
  f(y,false);
  print_int (!x) ;;
*)

(* let rec foo x = x ;;

let main () =
  let _ = foo (5:int<4>) in foo (5:int<4>) ;;*)

(* let rec bar(y,z) = 
  let x = foo z in 
  foo y ;;

let main () =
  bar ((5:int<5>),4);
  bar ((1:int<5>),2)
;;*)

(* let rec foo y =
  let z = ref y in
  !z ;;

let main () =
  let x = foo true in foo 6;;

  *)