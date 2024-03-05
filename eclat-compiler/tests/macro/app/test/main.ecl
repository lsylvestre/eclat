(* ./eclat ../examples/app/test/main.ecl -arg "false;false;false;true;true;false;false;false;false;false;false;true;true;true;false;false;false;false" *)

(* ./eclat ../examples/app/test/main.ecl -arg "false;false;false;true;true; \
                                               false;false;false;false;false; \
                                               false;false;false;false;false;false;false;true;true;true;false;false;false;false"
                                             *)

type t = 
   Idle of unit
  | WaitN of unit  
  | Go of int 
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
    if (n land 2) = 0 then loop(n lsr 1,t+1)
    else loop(n+n+n+1,t+1)
  in loop(n,1) ;;

(* let rec collatz_n(n) =
  (* if n < 1 then 5 else collatz_n(m,n-1) ;;*)
  (* if n < 1 then (if m >= 10 then (print_int m; collatz(m)) else m)
  else collatz_n(collatz(m),n-1) ;;
*)
  let c = collatz(n) in
  if c >= 10 then collatz_n(c) else c ;;
*)

let rec collatz_n(n) =
  let n = collatz(n) in
  if n < 5 then 1 else
  if n < 10 then 2 else
  if n < 15 then 3 else
  if n < 20 then 4 else
  if n < 25 then 5 else
  if n < 30 then 6 else
  if n < 35 then 7 else 
  if n < 40 then 8 else 9 ;;

let n = 3 ;;
let reset = false ;;



let proc((i,suspend) : bool * bool) =
  let ri = edge(i) in
  print_string "EDGE:"; print_int (if ri then 1 else 0);
  reg (fun state ->
    match state with
    | Idle() ->
       print_string "IDLE ";
       if ri then WaitN() else Idle()
    | WaitN() ->
       (* ********************* *)
       print_string "WaitN ";
       (* ********************* *)
       let n = counter(0,true,reset) in
       if ri then Go(n) else WaitN()
    | Go(n) -> 
        print_string "Go "; print_int n; print_string "|"; print_int n;
        (* ********************* *)
        if suspend then Go(n) else
        let (x,rdy) = exec collatz_n(n) default 9 in
        if rdy then S(x) else Go(n)
    | S x ->
        (* ********************* *) 
        print_string "S "; print_int x; 
        S x 
    end) last (Idle()) ;;


let foo(i:bool) =
  match proc(i,false) with
  | S x -> (false, x)
  | Go _ -> (false, 0)
  | WaitN() -> (true,0)
  | Idle() -> (false,0)
  end ;;

let mainI (i) =
  let (b,n) = foo(i) in
  print_string (if b then "true| " else "false|");
  print_int n;
  print_newline();
  (not b,display_x n) ;;


let main ((switches,keys) : inputs) : outputs =
 let (s0,s1,s2,s3,s4,s5,s6,s7,s8,s9) = switches in
(* ((false,false,false,false,false,false,false,false,false,false),
  let d = (display_x (counter(0,edge(not s0),false))) in (d,d,d,d,d,d)) ;;*)
  let (led1,d1) = mainI(not s0) in
  let (led2,d2) = mainI(not s1) in
  let (led3,d3) = mainI(not s2) in
  let (led4,d4) = mainI(not s3) in
  let (led5,d5) = mainI(not s4) in
  ((led1,led2,led3,led4,led5,false,false,false,false,false),
   (d1,d2,d3,d4,d5,display0)) ;;
