let get (x,y) = Array.get x y
let set (x,y,z) = Array.set x y z
let length = Array.length

(* ************************* *)

let sum_neighborhood(f,i,n) =
  f(i-n-1) + f(i-n) + f(i-n+1) + 
  f(i-1)            + f(i+1) +
  f(i+n-1) + f(i+n) + f(i+n+1) ;;
  
let next_cell(get_cell,w,i,cell,n) =
  let alive_int(i) = 
    if get_cell(w,i) then 1 else 0 in
  let s : int = sum_neighborhood(alive_int,i,n)
  in (cell & s = 2) or (s = 3) ;;

let pos_modulo(i,n,size) =
  if i < 0 then i+size else
  if i >= size then i-size else i ;;

let array_mapi (f,src,dst) =
 let rec aux(i) =
   if i < length(src) then
   (set(dst,i,f(i,get(src,i))); 
    aux(i+1)) else () 
 in aux(0) ;;

let array_life (src,dst,n) =
 let access(w,i) = 
  get(w,pos_modulo(i,n,length w)) 
 in  
 let f (i,cell) =
  next_cell(access,src,i,cell,n)
 in array_mapi(f,src,dst) ;;


let print_world (world,n) : unit =
  for i = 0 to n*n - 1 do
    if (i mod n) = 0 then print_newline () else ();
    print_string (if get(world,i) then "*" else "-")
  done;
  print_newline ();
  print_string "==============";
  print_newline () ;;

let copy(src,dst) =
  array_mapi((fun (_,x) -> x),src,dst) ;;


let d = 8 ;;

let main () =
  let w = Array.make (d*d) false in
  let w_dst = Array.make (d*d) false in

  w.(0) <- true;
  w.(2) <- true;
  w.(d+1) <- true;
  w.(d+2) <- true;
  w.(d+d+1) <- true;

  for i = 1 to 500 do
    print_world(w,d);
    array_life(w,w_dst,d);
    print_world(w_dst,d);
    array_life(w_dst,w,d);
  done ;;


main ();;

