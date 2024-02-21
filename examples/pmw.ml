let counter() = 
  let step(i) = if i = 100 then 0 else i + 1 in
  reg step last 0 ;;

let edge(i) =
  let step(_,pre_i) = (pre_i,i) in
  let (pre_i,_) = reg step last (false,false) in
  not(pre_i) & i ;;

let controller(inc,dec) = 
  let step c = if edge(inc) then c + 5 else
               if edge(dec) then c - 5 else c in
  reg step last 50 ;;

let pwm(f,inc,dec) =
  let r = controller(inc,dec) in
  let c = counter() in
  c <= f(r) ;;

let collatz(n) =
  let rec loop(n,t) =
    if n = 1 then t else
    if n mod 2 = 0 then loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(n,0) ;;

let rec wait n =
  if n < 1 then () else wait (n-1) ;;

let foo(x) =
  reg (fun o ->
    let (nexto,rdy) = exec wait(500000 * collatz(x)); x default o 
    in nexto) last 0 ;;

let main(inc,dec) =  
  pwm(foo,inc,dec);;


(* let counter() = 
  let step(i) = if i = 100 then 0 else i + 1 in
  reg step last 0 ;;

let edge(i) =
  let step(_,pre_i) = (pre_i,i) in
  let (pre_i,_) = reg step last (false,false) in
  not(pre_i) & i ;;

let controller(inc,dec) = 
  let step c = if edge(inc) then c + 5 else
               if edge(dec) then c - 5 else c in
  reg step last 50 ;;

let pwm(f,inc,dec) =
  let r = controller(inc,dec) in
  let c = counter() in
  c <= f(r) ;;

let f x = 
  x ;; 

let main(inc,dec) =  
  pwm(f,inc,dec);;*)


(*


let rec wait n =
  if n < 1 then () else wait (n-1) ;;

let pwm(r) =
  let step o =
    let t = if o then r else 100 - r in
    let (nexto,_) = exec wait(t); 
                         not(o) 
                    default o 
    in nexto
  in 
  reg step last false ;;


 let collatz(n) =
  let rec loop(n,t) =
    if n = 1 then t else
    if n mod 2 = 0 then loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(n,0) ;;

let edge i =
  let (pre_i,_) = reg (fun (_,pi) -> (pi,i)) last (false,false)
  in not(pre_i) & i ;;

let control(inc,dec) =
  reg (fun c -> if edge(inc) then c + 1 else
                if edge(dec) then c - 1 else c)
  last 100;;

let counter(max) =
  reg (fun c -> if c = max then 0 else c + 1) last 0 ;;

let update_r(i) =
  reg (fun v -> 
    let (nxt_v,rdy) = exec collatz(i) default v in nxt_v) 
  last 1 ;;

let main(inc,dec) =
  let i = control(inc,dec) in
  let c = counter(100) in
  let r = update_r(i) in
  c < r ;; *)




(* let collatz(n) =
  let rec loop(n,t) =
    if n = 1 then t else
    if n mod 2 = 0 then loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(n,0) ;;

let edge i =
  let (pre_i,_) = reg (fun (_,pi) -> (pi,i)) last (false,false)
  in not(pre_i) & i ;;

let update_r(v,w) =
  let (r,_) = if v then exec
  sustain (fun r -> if v then r + collatz(r) else
                    if w then r - collatz(r) else r)
    in r) last 50 ;;

let main(inc,dec) =
  let r = update_r(edge(inc), edge(dec)) in
  let c = reg (fun c -> if c = 100 then 0 else c + 1) last 0 in 
  (c < r) ;; *)



(* let fby (a,b) =
  let step (x,_) = (b,x) in
  let (_,x) = reg step last (a,b) in x ;;

let edge i = not (fby(false,i)) & i ;;

let count(inc,dec,step,init) =
  reg (fun c -> let c = if edge(inc) then c + step else c in
                let c = if edge(def) then c - step else c in
                c) last init ;;

let main (inc,dec) =
  let c = reg 0
  count(inc,dec,step,init)
  *)
  (* 
let main (i) = print_int (if edge i then 1 else 0);;


let rec wait n =
  if n < 1 then () else wait (n-1) ;;


let main(inc,dec) =
  let r = reg (fun (pi,de,c) ->
            if inc & not pinc then c + step in
            if dec & not dec then c - step in
            if dec then c-1 else c) last 0 in
  reg (fun o ->
    let (v,rdy) = exec 
                    wait (if o then r else 100 - r); not(o) 
                  default o
    in v)
  last false ;;*)

(* let pwm r =
  let i = reg (fun i -> if i = 100 then 0 else i+1) last 0 in
  i >= r ;;

let count(inc,dec) = 
  reg (fun c -> if inc then c + 1 else 
                if dec then c - 1 else c) last 0 ;;

let pwm_f(f,inc,dec) =
  let c = count(inc,dec) in
  let r = reg (fun r -> let (r2,rdy) = exec f(c) default r in r2) last 0 in
  pwm(r) ;;

let main(inc,dec) =
  let rec f x = x in *)



(*

let rec wait n =
  if n < 1 then () else wait (n-1) ;;

let main (low,high) =
  reg (fun o ->
    let (o,rdy) = 
      exec wait (if o then low else high); not(o) 
      default o
    in o)
  last false ;;




*)