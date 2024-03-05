(* let main () = 
  let v = ref 42 in
  let v2 = ref 55 in
  let w = v (* if false then v else v2 *) in
  v := 10;
  print_int (!(let z = 5 in 
              (if z = 5 then v := 12 else ()); v2 := 77; w)) ;;
*)


let main () =
  let ((u,x),f) = ((6+6,ref 0), fun y -> y) in
  let f = (fun (x2,x3) ->
    x2 := 5) in f (x,4);
    print_int (!x) ;;


(*let x = 0;;

let main n =
  let _ = x + (5: int<10>) in
  let _ = x + (4 : int<5>) in ();; *)
(* let main () =
 let x = ref 0 in
  let rec loop (i) =
    if i < 0 then (print_int(!x); print_newline ()) else (x := (!x + i); loop(i-1))
  in 
  loop(10:int<10>);
  loop(2:int<10>);;
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