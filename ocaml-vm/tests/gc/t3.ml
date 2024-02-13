open CustomStdlib ;;


(* heap_size = 20 *)

let rec map f l = 
  match l with
  | [] -> []
  | x::xs -> f x:: map f l ;;

let f1 x = x + 1 ;;
let f2 x = x + 2 ;;
let f3 x = x + 3 ;;
let f4 x = x + 4 ;;
let f5 x = x + 5 ;;
let f6 x = x + 6 ;;

let x = 1 ;;

let (@@) f x = f x ;;
 
map f1 @@
map f2 @@
map f3 @@
map f4 @@
map f5 @@
map f6 @@ [x;x;x;x;x;x] ;;