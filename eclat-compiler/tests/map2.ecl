(* $ ./eclat tests/map.ecl
   $ make simul NS=4000 
 
 ./eclat tests/map.ecl  -yosys-ecp5 -clk-top="clk48" -top="a:1|b:2"

 *)

(*
let counter () =
  reg (fun c -> c + 1) last 0 ;;


let main () =
  let cy = counter () in
  print_string "cy: "; print_int cy; print_newline ();
  exec
    let f (i,acc) = i + acc in
    print_int (generate (f,0) ~n:11);
    print_newline () 
  default ();;
*)



let static tab = 100^10;;
let static tab2 = 100^10;;


let reduce (f,init) ~n ~src =
  let rec loop(acc,i) =
    (* if i >= src.length - n then acc else*) 
    let g (ofs,acc) = src[i] + acc (* (src[i+ofs],acc)*) in
    let u = generate (g,acc) ~n:n in
    (* loop(u,i+n) *)
    u
  in
  loop(init,0) ;;
(*
let mapi f ~n ~src ~dst =
  let rec loop(i) =
    if i >= src.length - n then () else
    let update(i) = dst[i] <- f (i,src[i]) in
    spawn update ~n:n;
    loop(i+n)
  in
  loop(0) ;;

let min(x,y) = if x < y then x else y ;;

let zipWith f ~n ~src1 ~src2 ~dst =
  let length = min (src1.length, src2.length) in
  let rec loop(i) =
    if i >= length - n then () else
    let update(i) = dst[i] <- f (src1[i],src2[i]) in
    spawn update ~n:n;
    loop(i+n)
  in
  loop(0) ;;
(* let scan f ~n ~src ~dst =
  let rec loop(i) =
    if i >= src.length - n then () else
    let update(ancc,i) = f(acc,i) in
    fold update ~n:n;
    loop(i+n)
  in
  loop(0) ;; *)

(* let rec pause () = () ;;*) (* ail !! *)

let rec pause x = print_int x; print_newline (); x ;;

let sum n =
  let rec loop (acc,n) = if n < 1 then acc else loop(n+acc,n-1) in
  loop (0,n) ;;
*)
let counter () =
  reg (fun c -> c + 1) last 0 ;;

let foo (x,y) = x + y ;;

let main () =
  let cy = counter () in
  exec
    (* print_string "cy:";  print_newline (); *)
    (* let g = (fun (i,x) -> i) in
    let () = mapi g ~n:8 ~src:tab ~dst:tab in
    let () = zipWith (+) ~n:8 ~src1:tab ~src2:tab ~dst:tab in*)
    print_string "start: ";
    print_int cy;
    print_string "====> ";
    print_int (reduce ((+),0) ~n:5 ~src:tab);
    print_string "done:";
    print_int cy;
        print_newline ();
    ()
    (* print_string "cy:"; print_int c; print_newline ();
    (mapi (fun (i,x) -> print_string "==> "; print_int x; print_newline (); x)) ~src:tab2 ~dst:tab2;
    ()*)
  default ();;

