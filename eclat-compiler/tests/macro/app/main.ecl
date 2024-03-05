(* ./eclat ../examples/app/IOs.ecl ../examples/app/main.ecl -intel-max10 -noassert -noprint *)


(* ./eclat ../examples/app/IOs.ecl ../examples/app/main.ecl -main proc -arg "(false,false,false);(true,false,false);(false,false,false);(true,false,false);(false,false,false);(true,false,false);(false,false,false);(true,false,false);(false,false,false);(true,false,false);(false,false,false);(true,false,false);(false,false,false);(true,false,false);(false,false,false);(true,false,false);(false,false,false);(true,false,false);(false,true,false);(false,true,false);(false,true,false);(false,true,false);(false,true,false);(false,true,false);(false,true,false);(false,false,false);(false,true,false);(false,true,false);(false,true,false);(false,true,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,true,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false);(false,false,false)" *)
type t = 
    Wait1 of unit 
  | WaitM of unit 
  | WaitN of int  
  | Go of int * int 
  | S of int

let fby (a,b) =
  let step (x,_) = (b,x) in
  let (_,x) = reg step last (a,b) in x
;;

let edge i = not (fby(false,i)) & i ;;

let counter(init,b,reset) =
  reg (fun c -> 
    let c = if reset then init else c in
    if b then c + 1 else c) 
  last init ;;

let collatz(n) =
  let rec loop(n,t) =
    if n <= 1 then t else
    if n mod 2 = 0 then loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(n,1) ;;

let rec collatz_n(m,n) =
  (* if n < 1 then 5 else collatz_n(m,n-1) ;;*)
  (* if n < 1 then (if m >= 10 then (print_int m; collatz(m)) else m)
  else collatz_n(collatz(m),n-1) ;;
*)
  let c = collatz(m+1) in
  if c >= 10 then collatz_n(c,n) else c ;;

let n = 3 ;;
let reset = false ;;

let display_x x =
  if x = 1 then display1 else
 if x = 2 then display2 else
 if x = 3 then display3 else
 if x = 4 then display4 else
 if x = 5 then display5 else
 if x = 6 then display6 else
 if x = 7 then display7 else
 if x = 8 then display8 else
 if x = 9 then display9 else
 displayZ ;;

let proc(i, s,suspend) =
  let s2 = edge(s) in
  reg (fun state ->
    match state with
    | Wait1() -> 
        (* ********************* *)
        print_string "Wait1 ";
        (* ********************* *)
        if counter(0,edge(i),reset) >= n
        then WaitM()
        else Wait1()
    | WaitM() ->
       (* ********************* *)
       print_string "WaitM ";
       (* ********************* *)
       let m = counter(0,true,reset) in
       if s2 then WaitN(n) else WaitM()
    | WaitN(m) ->
       (* ********************* *)
       print_string "WaitN ";
       (* ********************* *)
       let n = counter(0,true,reset) in
       if s2 then Go(m,n) else WaitN(m)
    | Go(m,n) -> 
        print_string "Go "; print_int n; print_string "|"; print_int m;
        (* ********************* *)
        if suspend then Go(m,n) else
        let (x,rdy) = exec collatz_n(m,n) default 9 in
        if rdy then S(x) else Go(m,n)
    | S x ->
        (* ********************* *) 
        print_string "S "; print_int x; 
        S x 
    end) last (Wait1()) ;;


let foo(x) =
  match proc(x,x,false) with
  | S x -> display_x x
  | _ -> display0
  end ;;


let main ((switches,keys) : inputs) : outputs =
 let (s0,s1,s2,s3,s4,s5,s6,s7,s8,s9) = switches in
(* ((false,false,false,false,false,false,false,false,false,false),
  let d = (display_x (counter(0,edge(not s0),false))) in (d,d,d,d,d,d)) ;;*)
  let d1 = foo(not s0) in
  let d2 = foo(not s1) in
  let d3 = foo(not s2) in
  let d4 = foo(not s3) in
  let d5 = foo(not s4) in
  ((false,false,false,false,false,false,false,false,false,false),
   (d1,d2,d3,d4,d5,display0)) ;;
