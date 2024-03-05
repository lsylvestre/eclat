(* ./eclat ../examples/app/pipe.ecl -arg="1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9" *)

let pipe (f1,f2,x0) =
  let ((_,x2,_),rdy) = 
    reg (fun ((x1,x2,init),rdy) ->
          exec (let x1 = f1(x0) 
                and x2 = if init then x2 else f2(x1) in
                (x1,x2,false))
          default (x1,x2,init))
    last ((0,0,true),false)
  in
  (* (if r then (print_string "~~x2:"; print_int o; print_newline ()) else ()); *)
  (x2,rdy)
;;
(*
let pipe3 (f1,f2,f3,x) =
  let rec f12 x = let (y,_,valid) = pipe(f1,f2,x) in if valid then y else f3 x in
  pipe(f12,f3,x) ;;*)
(*
let pipe (f1,f2,x0) =
    let (r1,r2,_,_,_,x2,_) = 
    reg (fun (rdy1,rdy2,run1,run2,x1,x2,s) ->
      (* **************************************************** *)
      (* **************************************************** *)
      let go1 = rdy2 or run1 in
      let (x1,rdy1) = if go1 (* or s*) then (print_string "!"; exec f1 x0 default x1) else (x1,rdy1) in
      let go2 = (rdy1 or run2) in
      let (x2,rdy2) = if go2 (* rdy1 or run2*) (* or (s & rdy1)*) then (print_string "~"; exec f2 x1 default x2) else (x2,rdy2) in
      let run1 = go1 (* counter(rdy1,false) <= 1 *) in
      let run2 = go2 (* counter(rdy2,false) <= 1*) in
      let (run1,run2) = if run1&rdy1&run2 then (false,true) else (run1,run2) in
      let (run1,run2) = if run2&rdy2&run1 then (true,false) else (run1,run2) in
      (* **************************** *)
      print_string "|rdy1:"; print_int (if rdy1 then 1 else 0);
      print_string "|rdy2:"; print_int (if rdy2 then 1 else 0);
      print_string "|run1:"; print_int (if run1 then 1 else 0);
      print_string "|run2:"; print_int (if run2 then 1 else 0);
      print_string "|x0:"; print_int x0;
      print_string "|x1:"; print_int x1;
      print_string "|x2:"; print_int x2;
      print_newline ();
      (* **************************** *)
      (* let (run1,run2) = if not(run1)&not(run2) then (false,true) else (run1,run2) in*)
    (rdy1,rdy2,run1 or rdy2,run2,x1,x2,false)
  ) last (false,true,true,false,0,0,true) in
  (x2,not(r1), r2)
;;
*)

let wait (n) =
  let rec loop (m) = if m < 1 then n else loop(m - 1) in loop(n) ;;

let sum n = 
  let rec loop (acc,n) = 
    if n < 1 then acc else loop(acc+n,n-1) 
  in loop(0,n) ;;


let f1 x = (*let _ = wait(1) in *)x * 2 ;;
let rec f2 x = x + 1 ;;
let rec f3 x = x * 100 ;;

let static tab = 0^10;;
 
let h _ = tab[0] ;;
let h2 _ = tab[1] ;;


let main x0 =
  print_string "|x0:"; print_int x0;
  let (o,rdy) = (* pipe(wait,wait,x0) *) pipe(h,h2,x0) in
  print_string "|rdy:"; print_int (if rdy then 1 else 0);
  (* print_string "|valid:"; print_int (if valid then 1 else 0);print_newline (); *)
  (if rdy then (print_string "===> x2: "; print_int o;print_newline ()) else ());
  (* print_string "|x0:"; print_int x0;
  print_string "|go:"; print_int (if go then 1 else 0);
  print_string "|rdy:"; print_int (if rdy then 1 else 0);
  print_string "|y:"; print_int y;
  print_newline ();*)
  ()
   ;;

(* let main x0 =
  reg (fun (rdy1,x1,x2) ->
    let (x2,rdy2) = if rdy1 then (exec f2 x1 default 0) else (0,true) in
    let (x1,rdy1) = if rdy2 then (exec f1 x0 default 0) else (0,true) in
    print_string "|rdy1:"; print_int (if rdy1 then 1 else 0);
    print_string "|rdy2:"; print_int (if rdy2 then 1 else 0);
    print_string "|x1:"; print_int x1;
    print_string "|x2:"; print_int x2;
    print_newline ();
    (rdy1,x1,x2)
  ) last (false,0,0)
;;*)