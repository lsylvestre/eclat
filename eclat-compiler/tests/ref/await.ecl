(* let main () =
  let r = ref 5 in
let fact n = 
   let rec aux n = aux n + !r
in aux 5 in
fact (5:int<5>) + fact (5:int<5>);;*)
let await i =
  let rec loop () =
    if !i then () else loop ()
  in loop () ;;

let foo x = 
  let rec loop() =
    await(x); await(x) in loop ();; 

let main () =
  let a = ref false in
  foo(a) ;;

(* 
./eclat tests/ref/abro.ecl  -relax -top="a:1,b:1,c:1,r:1|o:1" \
   -arg="(false,false,false,true);(false,false,false,true);(true,true,true,true)"

  ./eclat tests/ref/abro.ecl  -relax -top="a:1,b:1,c:1,r:1|o:1" -arg "(false,false,false,true);(false,false,false,false);(false,false,false,false);(true,true,true,false);(true,true,true,false);(false,false,false,true);(false,false,false,false);(true,true,true,false);(false,false,false,false)" *)

(*    
let rec wait(n) = 
  if n < 0 then () else wait(n-1) ;;

let repeat f = 
  let rec loop () =
    f (); 
    loop ()
  in loop () ;;
*)
(* let main () =
  let a = ref false in
  let b = ref false in
  let c = ref false in
  let r = ref false in
  let o = ref false in
  abcro(a,b,c,r,o);;*)
  (* let () = r := true; wait(1); r := false
  and () = abro (a,b,r,o)
  and () = wait(3); a := true; wait(2); a := false; 
           wait(10); b := true; wait(1); b := false; print_string "bar"
  and () = wait(10); b := true; wait(1); b := false; 
           wait(15); a := true; wait(2) ; a := false; print_string "foo"
  and () = repeat (fun () -> print_int (if !o then 1 else 0); print_newline ())
  in
  () ;;*)


(*
let z = let i = 6 + 6 in ((fun y -> y + i), let v = ref 2 in v) ;;
let v = let u = ((z,z),z) in let (x,_) = u in x ;;
let main () =
  let ((f,_) ,_) = v in f 4 ;;*)

(*
let main () =
 let x = ref 0 in
 let y = ref true in
  let f(r,v) =
    r := v
  in 
  f(x,42);
  f(y,false);
  print_int (!x) ;;
*)

(* let rec foo x = x ;;

let main () =
  let _ = foo (5:int<4>) in foo (5:int<4>) ;;*)

(* let rec bar(y,z) = 
  let x = foo z in 
  foo y ;;

let main () =
  bar ((5:int<5>),4);
  bar ((1:int<5>),2)
;;*)

(* let rec foo y =
  let z = ref y in
  !z ;;

let main () =
  let x = foo true in foo 6;;

  *)