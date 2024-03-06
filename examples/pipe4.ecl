(* ./eclat ../examples/demi_pipe.ecl
   make simul NS=60000 *)
   
let static a1 = 10^100 ;;
let static a2 = 0^100 ;;

let map (f,src,dst) = 
  let rec loop(i) =
    if i >= dst.length then () else
    (dst.(i) <- f (src.(i)); loop(i+1))
  in loop(0) ;;

let print_id (n,iteration) =
  print_string "===>"; print_int n; 
  print_string "/"; print_int iteration; 
  print_newline();;

(** val demi_pipe : int * ('a -> 'b) * 'a array * 'b array -> unit *)
let demi_pipe(id,f,src,dst) =
  let rec loop(i) =
    if i >= dst.length then () else
    let (v,b) = src.(i) in
    if b then ((* print_id(id,i); *)
         let () = src.(i) <- (v,false)
         and () = dst.(i) <- f v in
         loop(i+1)) else loop(i)
  in loop(0) ;;

(** val pipe2 : int * (('a -> 'b) * ('b -> 'c)) * 'b * ('a array * 'c array) -> unit *)
let pipe2 (id,(f1,f2),d,(src,dst)) =
  let tmp = (d,false)^(src.length) in
  let () = map ((fun v -> (f1 v,true)),src,tmp)
  and () = demi_pipe(id,f2,tmp,dst) in
  () ;;

let pipe3 (id,(f1,f2,f3),(d1,d2),(src,dst)) =
  let tmp = (d2,false)^(src.length) in
  let g2 v = (f2 v,true) in
  let () = pipe2(id,(f1,g2),d1,(src,tmp))
  and () = demi_pipe(id+2,f3,tmp,dst) in
  () ;;

(* ******************************* *)

(* V1 *)

let pipe4 (id,(f1,f2,f3,f4),(d1,d2,d3),(src,dst)) =
  let tmp = (d3,false)^(src.length) in
  let g3 v = (f3 v,true) in
  let () = pipe3(id,(f1,f2,g3),(d1,d2),(src,tmp))
  and () = demi_pipe(id+3,f4,tmp,dst) in
  () ;;

(* V2 *)

let pipe4_bis (id,(f1,f2,f3,f4),(d1,d2,d3),(src,dst)) =
  let tmp1 = (d1,false)^(src.length) in
  let tmp2 = (d2,false)^(src.length) in
  let tmp3 = (d2,false)^(src.length) in
  let g1 v = (f1 v,true) in
  let g2 v = (f2 v,true) in
  let g3 v = (f3 v,true) in
  let () = map (g1,src,tmp1)
  and () = demi_pipe(id,g2,tmp1,tmp2)
  and () = demi_pipe(id+1,g3,tmp2,tmp3)
  and () = demi_pipe(id+2,f4,tmp3,dst) in
  () ;;

(* V3 : pipe4 séquentiel (pour voir le facteur d'accélération) *)

let version_naive4 (_,(f1,f2,f3,f4),_,(a1,a2)) =
  let rec loop(i) =
    if i >= a2.length then () else
    let v = a1.(i) in
    (a2.(i) <- (f4(f3(f2(f1(v))))); loop(i+1))
  in loop(0); print_newline () ;;


let display_array a =
  let rec loop (i) =
    if i >= a.length then () else 
    (print_int (a.(i)); loop(i+1))
  in loop(0); print_newline () ;;

let rec wait(n) =
  if n <= 1 then () else wait(n-1) ;;

let f1 x = wait(10); x + 2 ;;
let f2 x = wait(2); x * 2 ;;
let f3 x = wait(5); x - 1 ;;
let f4 x = wait(1); x * 10 ;;

let counter () =
  reg (fun c -> c+1) last 0 ;;

let print_cy cy =
  print_string "cy="; print_int cy; print_newline () ;;

(* ********************* *)

let version = 1 ;; 

let main () =
  let cy = counter () in
  let ((),rdy) = exec
    print_cy cy;
    (* ************** *)
    
     pipe4(1,(f1,f2,f3,f4),(0,0,0),(a1,a2))
     (* pipe4_bis
     version_naive4*)
    (* ************** *)
    
    (* ; display_array(a2) *)
  default () in
  (if rdy then print_cy cy else ()) ;;
