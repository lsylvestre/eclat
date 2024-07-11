let map_slice(a,b,f,src,dst) = 
  let rec aux(i) =
    if i < b then 
      (set(dst,i,f(get(src,i)));
       aux(i+1))
    else ()
  in 
  if a >= b then () else aux(a) ;;

let par_map(p,f,src,dst) =
  let n = length src in
  let d = n/p in
  macro_for i = 0 to p-1 do
   map_slice(d*i,d*(i+1),f,src,dst)
  done;
  map_slice(d*p,n,f,src,dst);;

let map((p,f,src),k) =
  let dst = create (length src) in 
  par_map(p,f,src,dst); k(dst) ;;


(* ********************************* *)

(* simulate a computation of fixed duration [n] *)
let rec wait(n) =
  if n <= 1 then () else wait(n-1) ;;

(* ********************************* *)

let collatz(n) =
  let rec loop(n,t) =
    if n = 1 then t else
    if n mod 2 = 0 then loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(n,1) ;;


let f x = wait(30); x ;;

let init_array(a) =
  for i = 0 to length a - 1 do
    set(a,i,i)
  done ;;

let counter () =
  reg (fun c -> c + 1) last (-1) ;;

let main () =
  let cy = counter () in
  let (_,rdy) = 
    exec
      let a = array_create (3200) in
      (* init_array(a) *)
      let p =20  in
      (map(p,f,a) @@ fun t -> ())
  default () in
if rdy then (print_string " cy="; print_int cy; print_newline ()) ;;
