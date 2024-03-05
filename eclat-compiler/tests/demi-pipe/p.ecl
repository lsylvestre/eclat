let static a1 = (10,false)^10 ;;
let static a2 = (0,false)^10 ;;
let static a3 = (0,false)^10 ;;
let static a4 = (0,false)^10 ;;

let demi_pipe(id,go,f,src,dst) =
  let rec loop(i) =
    if i >= dst.length then () else
    let (v,b) = src.(i) in
    (* print_string "~~~~>"; print_int(if b then 0 else 1); *)
    if go or b then (print_string "===>"; print_int id; print_string "/"; print_int i; print_newline();
               src.(i) <- (v,false);
               dst.(i) <- (f v,true);
               print_int id; print_string " done"; print_newline();
               loop(i+1)) else loop(i)
  in loop(0) ;;

let display_array a =
  let rec loop (i) =
    if i >= a.length then () else 
    (let (v,_) = a.(i) in
      print_int (v); loop(i+1))
  in loop(0); print_newline () ;;

let rec wait(n) =
  if n <= 1 then () else wait(n-1) ;;

let f1 x = (* wait(1);*) x + 2 ;;
let f2 x = (*wait(1);*) x * 2 ;;
let f3 x = (*wait(1);*) x - 1 ;;

let counter () =
  reg (fun c -> c+1) last 0 ;;

let print_cy cy =
  print_string "cy="; print_int cy; print_newline () ;;

let pipe3 () =
  let () = demi_pipe(0,true,f1,a1,a2)
  and () = demi_pipe(1,false,f2,a2,a3)
  and () = demi_pipe(2,false,f3,a3,a4) in
  () ;;

let naive () =
  let rec loop(i) =
    if i >= a4.length then () else
    let (v,_) = a1.(i) in
    (a4.(i) <- (f3(f2(f1(v))),true); loop(i+1))
  in loop(0); print_newline () ;;

let main () =
  let cy = counter () in
  let ((),rdy) = exec
    (* pipe3 *)
    print_cy cy;
    naive()
  default () in
  (if rdy then print_cy cy else ()) ;;
  (* display_array(a4);;*)
