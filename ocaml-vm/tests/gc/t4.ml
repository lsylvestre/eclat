open CustomStdlib ;;


(* heap_size = 50 *)

let rec f l =
  match l with
  | [] -> []
  | x::xs -> x :: f xs ;;


let rec print_list l =
  match l with
  | [] -> ()
  | x::xs -> print_int x; (print_list xs) ;;



let x = 42 ;;

let (@@) f x = f x ;;

print_list @@ 
f @@
f @@ 
f @@
f @@
f @@ 
f @@
f @@
f @@ 
f @@
f @@
f @@ 
f @@
f @@
f @@ 
f @@
f ([x;x+1;x+2]) ;;
