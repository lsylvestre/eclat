let rec f x = f(x + 1) ;;

let main () =
  f(f(42)) ;;



let rec f x = x;;

let main () =
  (f true, f 5) ;;


let rec f x = 42 ;;

let main () : int<5> =
   f(f(f((f(55):int<6>)))) ;;


let compose ((f,g),x) = 
  f(g(x)) ;;

let f1 : int => int = fun y -> y+ 1;;
let f2 z = z * 3 ;;

let main () =
  compose((f1,f2),42) ;;