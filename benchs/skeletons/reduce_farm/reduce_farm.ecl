let reduce_worker (f,r,v0,src) =
  let rec loop(acc) =
    let i = get(r,0) in 
    if i < length src then (
      let () = set(r,0,i+1)
      and v = get(src,i) in
      loop (f(v,acc))) else acc
  in loop(v0);;

let reduce_par1(f,v0,src) =
  let r = create 1 in
  set(r,0,0);
  let x1 = reduce_worker(f,r,v0,src) in
  x1 ;;

let reduce_par2(f,v0,src) =
  let r = create 1 in
  set(r,0,0);
  let x1 = reduce_worker(f,r,v0,src)
  and x2 = reduce_worker(f,r,v0,src) in
  f(x1,x2) ;;

let reduce_par3(f,v0,src) =
  let r = create 1 in
  set(r,0,0);
  let x1 = reduce_worker(f,r,v0,src)
  and x2 = reduce_worker(f,r,v0,src)
  and x3 = reduce_worker(f,r,v0,src) in
  f(x1,f(x2,x3)) ;;

let reduce_par4(f,v0,src) =
  let r = create 1 in
  set(r,0,0);
  let x1 = reduce_worker(f,r,v0,src)
  and x2 = reduce_worker(f,r,v0,src)
  and x3 = reduce_worker(f,r,v0,src)
  and x4 = reduce_worker(f,r,v0,src) in
  f(x1,f(x2,f(x3,x4))) ;;

let reduce_par5(f,v0,src) =
  let r = create 1 in
  set(r,0,0);
  let x1 = reduce_worker(f,r,v0,src)
  and x2 = reduce_worker(f,r,v0,src)
  and x3 = reduce_worker(f,r,v0,src)
  and x4 = reduce_worker(f,r,v0,src)
  and x5 = reduce_worker(f,r,v0,src) in
  f(x1,f(x2,f(x3,f(x4,x5)))) ;;

let reduce_par6(f,v0,src) =
  let r = create 1 in
  set(r,0,0);
  let x1 = reduce_worker(f,r,v0,src)
  and x2 = reduce_worker(f,r,v0,src)
  and x3 = reduce_worker(f,r,v0,src)
  and x4 = reduce_worker(f,r,v0,src)
  and x5 = reduce_worker(f,r,v0,src)
  and x6 = reduce_worker(f,r,v0,src) in
  f(x1,f(x2,f(x3,f(x4,f(x5,x6))))) ;;

let reduce_par7(f,v0,src) =
  let r = create 1 in
  set(r,0,0);
  let x1 = reduce_worker(f,r,v0,src)
  and x2 = reduce_worker(f,r,v0,src)
  and x3 = reduce_worker(f,r,v0,src)
  and x4 = reduce_worker(f,r,v0,src)
  and x5 = reduce_worker(f,r,v0,src)
  and x6 = reduce_worker(f,r,v0,src)
  and x7 = reduce_worker(f,r,v0,src) in
  f(x1,f(x2,f(x3,f(x4,f(x5,f(x6,x7)))))) ;;


let reduce_par8(f,v0,src) =
  let r = create 1 in
  set(r,0,0);
  let x1 = reduce_worker(f,r,v0,src)
  and x2 = reduce_worker(f,r,v0,src)
  and x3 = reduce_worker(f,r,v0,src)
  and x4 = reduce_worker(f,r,v0,src)
  and x5 = reduce_worker(f,r,v0,src)
  and x6 = reduce_worker(f,r,v0,src)
  and x7 = reduce_worker(f,r,v0,src)
  and x8 = reduce_worker(f,r,v0,src) in
  f(x1,f(x2,f(x3,f(x4,f(x5,f(x6,f(x7,x8))))))) ;;


(* sequential version (for comparaison) *)
let reduce(f,v0,a) =
  let rec aux(acc,i) = 
    if i < (length a) then
      let x = get(a,i) in 
      aux(f(x,acc),i+1)
    else acc
  in aux(v0,0) ;;


let rec wait(n) =
  if n <= 1 then () else wait(n-1) ;;

let f (x,y) = wait(20); x+y ;;


let counter () =
  reg (fun c -> c + 1) last (-1) ;;


let init_array a =
 for i = 0 to length(a)-1 do set(a,i,i+1) done ;;

let main (_:int<12>) : int<58> =
  let cy = counter () in
  let (o,rdy) = 
    exec
      let a = array_create (4096) in
      init_array(a);
      (let v = reduce_par8((+),0,a) in 
        v)
    default 0
  in
  (if rdy then (print_string " cy="; 
               print_int (cy); 
               print_newline()) else ()); o ;;
