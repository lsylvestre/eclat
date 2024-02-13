open CustomStdlib ;;

(* heap_size = 20 *)

let rec loop i =
   if i < 0 then () else 
   let x = 42 in
   let (z,_) = 
     let y = (x,x) 
     in y 
   in
   print_int z;
   loop(i-1)
in 
loop (100) ;;

(*
let x = 42 ;;
let (z,_) = let y = (x,x) in y ;;
 print_int z ;; 

let x = 43 ;;
let (z,_) = let y = (x,x) in y ;;
 print_int z ;; 

let x = 44 ;;
let (z,_) = let y = (x,x) in y ;;
 print_int z ;; 


let x = 45 ;;
let (z,_) = let y = (x,x) in y ;;
 print_int z ;; 

let x = 46 ;;
let (z,_) = let y = (x,x) in y ;;
 print_int z ;; 


let x = 47 ;;
let (z,_) = let y = (x,x) in y ;;
 print_int z ;; 


let x = 48 ;;
let (z,_) = let y = (x,x) in y ;;
 print_int z ;; 


let x = 49 ;;
let (z,_) = let y = (x,x) in y ;;
 print_int z ;; 
*)