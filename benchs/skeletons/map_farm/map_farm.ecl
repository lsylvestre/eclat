let map_worker (f,r,src,dst) =
  let rec loop() =
    let i = get(r,0) in    
    if i < length src then (
      let ((),v) = (set(r,0,i+1) ||
                    get(src,i)) in
      set(dst,i,f(v)); 
      loop())
    else ()
  in loop();;

let map_farm ((p, f, src), k) = 
  
  let _ = (src : `A array<'N>) in (* type constraint *)
  
  let dst = create<'N> () in
  let r = create<1> () in
  set(r,0,0);
  parfor i = 0 to p - 1 do
    map_worker(f,r,src,dst)
  done;
  k(dst) ;;

(* *********************** *)

let collatz(n) =
  let rec loop(n,t) =
    if n <= 1 then t else
    if n mod 2 = 0 then loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(n,1) ;;

let rec wait(n) =
  if n <= 1 then () else wait(n-1) ;;

let f x = wait(60); x ;;


let init_array(a) =
  for i = 0 to length a - 1 do
    set(a,i,i+1)
  done ;;


let counter () =
  reg (fun c -> c + 1) last 0 ;;

let main () =
  let cy = counter () in
  let (_,rdy) = 
    exec
      let a = create<100> () in

      init_array(a); (* note: initialization takes time *)
    
      let p = 2 (* degree of parallelism *)
      in 
      (map_farm(p,collatz,a) @@ fun t -> ()) 
  default () in
if rdy then (print_string " cy="; 
             print_int (cy); 
             print_newline()) ;;
