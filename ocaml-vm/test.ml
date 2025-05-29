(*let x = [43;44];;

let y = 42::x;;

let f u = 
match y with
| [] -> 0
| a::r::z::_ -> z;;

f 5;;

let z = 0 in
let rec odd a b n = if n < 0 then 42 else even a (z+n-1)
and even a n = if n = 1 then 43 else odd a () (z+n-1) in

print_int (even () (11));;
*)

(*
let f x = x;;
let rec odd n = print_int n; if n < 0 then 42 else f (even (n-1))
and even n = print_int 66; if n = 1 then 35 else f (odd (n-1)) ;;
(* and even2 n = if n = 1 then 17 else f (even (n-1));;*)
print_int (even(10));;
*)


(*
let rec f1 () = f2()
and f2 () = f3 ()
and f3 () = f4 ()
and f4 () = f1 () ;;

f1 ();;
*)

(*

  Fatal error: exception Failure("invalid bytecode: at instruction index 15: invalid PUSHOFFSETCLOSURE parameter: -9 (not a multiple of 2)")

*)



let merge_sort list =
  let rec merge_one acc = function
    | ([], []) -> List.rev acc
    | (xs, [])
    | ([], xs) -> List.rev_append acc xs
    | ((x::xs as l1), (y::ys as l2)) ->
      if x < y
      then merge_one (x::acc) (xs, l2)
      else merge_one (y::acc) (l1, ys)
  in
  let rec merge acc = function
    | xs::ys::tl -> merge ((merge_one [] (xs, ys))::acc) tl
    | tl -> tl @ acc
  in
  let rec loop = function
    | _::_::_ as list -> loop (merge [] list)
    | list -> list
  in
  loop (List.map (fun x -> [x]) list);;



let cmp = fun x y -> if x < y then -1 else if x > y then 1 else 0 ;;
(*
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> List.rev_append l2 accu
    | l1, [] -> List.rev_append l1 accu
    | h1::t1, h2::t2 ->
        if cmp h1 h2 <= 0
        then rev_merge t1 l2 (h1::accu)
        else rev_merge l1 t2 (h2::accu)
*)


let l = List.sort cmp [1;5;2;3];;
List.iter (fun x -> print_int x) l ;;
(*


 let l = merge_sort 
  [1;5;4;10;2;8;0];;

List.iter (fun x -> print_int x) l ;;
*)

(*module M = Map.Make(struct 
             type t = int 
             let compare x y =
               if x < y then -1 else if x > y then 1 else 0
             end);;

let m = M.add 42 43 M.empty ;;

print_int (M.find 42 m) ;;
*)
(*
let t = Stack.create () ;;

Stack.push 5 t;;

Stack.push 6 t;;
Stack.push 7 t;;

print_int (Stack.top t);;
print_int (Stack.pop t);;
print_int (Stack.pop t);;
print_int (Stack.pop t);;

try print_int (Stack.pop t) with Stack.Empty -> print_int 42;;
*)

(* let l2 = List.stable_sort (fun x y -> if x < y then -1 else if x > y then 1 else 0)

        [1;2;5;42;6;42;7];;

List.iter (fun x -> print_int x) l2 ;;*)
(*
*)
(*

let f y = 
   let x =y in
  (x+1);;

print_int (f 5);;
 *)

(*
let z = 5 in

let f y () = 
  print_int (if 2 < 1 then 1 else let x = y + z in x - 1) in


f 42 ();;*)

(*
let f a b c = a + b + c ;;

print_int (f 6 10 3);;
*)

(*

let z = 5 in

let f y () = 
  print_int (if 1 > 2 then 1 else let x = y + z in x - 1) in


f 42 ();;*)
(*
type t = D of int * int * int ;;

match D (42,7,6) with
| D (n,m,t) -> print_int m;;*)


(*******
type r = {a:int;b:int}
;;

let y = 5 in
let x = {a=y;b=6} in
print_int x.b;;
***********)

(*
type t = A | B | C | D of int ;;
 let () =
match D 42 with
| A -> print_int 0
| B -> print_int 1
| C -> print_int 2
| D n -> print_int n;;
*)

(* ko *)

(*let f a b = a + b ;;

let g = f 5;; 

print_int (g 10) ;;*)

(*
let print_bool x =  print_int (if x then 42 else 17) ;;
print_bool (1 < 2);;
print_bool (1 <= 2);;
print_bool (1 <= 1);;
print_bool (1 = 1);;
print_bool (1 <> 2);;
print_bool (2 <> 1);;
print_bool (1 >= 1);;
print_bool (2 >= 1);;


print_bool (1 <> 1);;
print_bool (1 < 1);;
print_bool (1 > 1);;
*)

(*
let z = 4 + 1;;

let y = 1 + 1;;

let rec f a b = a + b + y ;;

let rec g = f z;; 

print_int (g (z+z)) ;;
*)



(* let rec odd n = (* if n < 0 then 42 else *) even (n-1)
and even n = 5 ;; (* if n = 1 then 42 else odd (n-1) ;;*)
*)

(*let rec odd n = if n < 0 then 42 else even (n-1)
and even n = if n = 1 then 17 else odd (n-1) ;;
print_int (even(10));;*)

(*
let rec f x =
  let y = Array.make 100 0 in
   f x;;

f ();;*)



(*
 let rec f91 n = 
      if n > 100 then n - 10 else 
      f91 (f91 (n+11)) ;;


print_int (f91 20);;*)




(*
let abs_sub (a, b) = if b > a then b-a else a-b;;

let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;

let rec safe_all_aux (qs, i, n, p) =
  print_int 1111111;
  if i > n then true
  else 
    ( if safe(qs.(i-1), p, n+1-i) 
      then (safe_all_aux(qs, i+1, n, p))
      else false 
    ) ;;

let safe_all (q,n,p) = safe_all_aux (q,1,n,p) ;;

let rec loop (col, i, n, v, nb) = 
 if i <= n then (
   if safe_all (v,col,i) then 
     ( print_int 12345678;
       v.(col) <- i ;
       if col+1 = n then print_int 42 else
       loop(col+1, 1, n, v, nb)  
     )
   else 
     loop(col, i+1,n, v, nb) )
  else 
   ( if col >= 1  (* <----- c'est un 2 ici *) 
     then ( loop(col-1, v.(col-1)+1, n, v, nb)) else ())
  ;;

let queens n = 
  let chess = Array.create n 1 in
  (loop (1, 1, n, chess, 0));;

queens(7);;*)

(*
let rec tak (x, y, z) =
 print_int x;print_int y;   print_int z;  
  if x > y then tak (tak (x - 1, y, 0), tak (y - 1, z, 0), 1)
  else z ;;


print_int 42;;

tak(18, 12, 6) ;;
*)

(*
print_int 42 ;;

  let y = 5 in
  let x1 = (y,y,y) in
  let x2 = (y,y,y) in

  let (a1,b,c) = x1 in
  let (a2,b,c) = x2 in
  print_int a1;
  print_int a2;;
*)
(* let rec tak (x, y, z) =
  (* print_int x; print_string "|"; print_int y;  print_string "|"; print_int z;   print_newline (); *)
  print_int x; print_int y; print_int z; print_int 666; 
  if x > y then tak (tak (x - 1, y, z), tak (y - 1, z, x), tak (z - 1, x, y))
  else z ;;*)


(*
let f x y = x + y;;

let g = f 2;;

print_int (g 4);;*)

(*print_int 42;;

let x = 4;;
let y = (1,x);;

let (a,b) =y ;;
print_int a;;*) 
(******
let y = 55 ;;
let f x a = x + a + y;;

let g = f 6;;

let _ = (g 5, g 7) ;;
print_int (f 2 y);;
********)
(*
print_int (if 4 < 5 then 42 else 43);;*)

(*
let y = 1 ;;

let rec f a b = 
  if a < 1 then b else f (a-y) (b+1) ;;

print_int (f 2 3);;*)
