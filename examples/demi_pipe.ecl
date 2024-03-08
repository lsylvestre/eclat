(* ./eclat ../examples/demi_pipe.ecl
   make simul NS=60000 *)
   
let static a1 = (10,false)^100 ;;
let static a2 = (0,false)^100 ;;

let demi_pipe(id,go,f,src,dst) =
  let rec loop(i) =
    if i >= dst.length then () else
    let (v,b) = src.(i) in
    if go or b then ((* print_string "===>"; print_int id; print_string "/"; print_int i; print_newline(); *)
               let () = src.(i) <- (v,false)
               and () = dst.(i) <- (f v,true) in
               (* print_int id; print_string " done"; print_newline(); *)
               loop(i+1)) else loop(i)
  in loop(0) ;;

let pipe4 ((f1,f2,f3,f4),(src,dst)) =
  let tmp1 = (0,false)^100 in
  let tmp2 = (0,false)^100 in
  let tmp3 = (0,false)^100 in
  let () = demi_pipe(0,true,f1,src,tmp1)
  and () = demi_pipe(1,false,f2,tmp1,tmp2)
  and () = demi_pipe(2,false,f3,tmp2,tmp3)
  and () = demi_pipe(2,false,f4,tmp3,dst) in
  () ;;

let naive ((f1,f2,f3,f4),(a1,a2)) =
  let rec loop(i) =
    if i >= a2.length then () else
    let (v,_) = a1.(i) in
    (a2.(i) <- (f4(f3(f2(f1(v)))),true); loop(i+1))
  in loop(0); print_newline () ;;


let display_array a =
  let rec loop (i) =
    if i >= a.length then () else 
    (let (v,_) = a.(i) in
      print_int (v); loop(i+1))
  in loop(0); print_newline () ;;

let rec wait(n) =
  if n <= 1 then () else wait(n-1) ;;

let f1 x = wait(1); x + 2 ;;
let f2 x = wait(1); x * 2 ;;
let f3 x = wait(1); x - 1 ;;
let f4 x = wait(1); x * 10 ;;

let counter () =
  reg (fun c -> c+1) last 0 ;;

let print_cy cy =
  print_string "cy="; print_int cy; print_newline () ;;

let pipelined = false ;; 

let main () =
  let cy = counter () in
  let ((),rdy) = exec
    print_cy cy;
    (* ************** *)
    (if pipelined 
    then pipe4 ((f1,f2,f3,f4),(a1,a2)) 
    else naive ((f1,f2,f3,f4),(a1,a2)))
    (* ************** *)
    
    (*; display_array(a5)*)
  default () in
  (if rdy then print_cy cy else ()) ;;
