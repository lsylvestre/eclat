(* ocamlfind ocamlopt -linkpkg -package domainslib test.ml -O3 *)

let num_domains = 
  if Array.length Sys.argv < 1 then (
    Printf.printf "one argument expected\n";
    exit 1)
  else
    let integer_expected () = 
      Printf.printf "positive integer expected (the number of cores)\n";
      exit 1 
    in
    match int_of_string_opt @@ Sys.argv.(1) with
    | None -> integer_expected ()
    | Some n -> if n <= 0 then integer_expected () else n
;;

let get (a,i) = Array.get a i
let set(a,i,v) = Array.set a i v
let create(n) = Array.make n false;;


let sum_neighborhood(alive,g,i,j,nbc,nbl) =
 alive(g,i-1,j-1,nbc,nbl)+alive(g,i,j-1,nbc,nbl)+alive(g,i+1,j-1,nbc,nbl) +
 alive(g,i-1,j,nbc,nbl)  +                       alive(g,i+1,j,nbc,nbl) +
 alive(g,i-1,j+1,nbc,nbl)+alive(g,i,j+1,nbc,nbl)+alive(g,i+1,j+1,nbc,nbl)
;;
let border(k,nb) = if k = (-1) then nb - 1 else (if k = nb then 0 else k) ;;

let alive_with(read_cell,(g,i,j,nbc,nbl)) = 
  if read_cell(g, border(i,nbc), border(j,nbl), nbl) then 1 else 0 ;;

let read_cell(src, i, j, nbl) = 
  get(src, i + j * nbl) ;;
 
let set_cell(dst, i, j, nbl, v) = 
  set(dst, i + j * nbl, v) ;;
        
let copy (src,dst) =
 let rec aux(i) =
   if i < Array.length(src) then 
     (set(dst,i,get(src,i)); 
      aux(i+1)) else () 
 in aux(0) ;;

let print_world (world,n) : unit =
  for i = 0 to n*n - 1 do
    if (i mod n) = 0 then print_newline () else ();
    print_string (if get(world,i) then "*" else "-")
  done;
  print_newline ();
  print_string "==============";
  print_newline () ;;




(* the sequential version *)
let array_life_seq (src, dst,nbc, nbl) =
 let rec loop(i,j) =
  if i = nbc then () else
  if j = nbl then loop(i+1,0) else (
  let cell = read_cell(src,i,j,nbl) in
  let alive(args) = 
    alive_with(read_cell,args) in 
  let sum  = sum_neighborhood
    (alive,src,i,j,nbc,nbl) in
  let new_cell = 
   (cell && sum = 2) || (sum = 3) in
  set_cell(dst,i,j,nbl,new_cell);
  loop(i,j+1)
 ) in loop(0,0);
 copy(dst,src) ;;

(* multicore version *)

open Domainslib

let pool = Task.setup_pool ~num_domains:(num_domains-1) ();;

let array_life (src, dst, nbc, nbl) =
 Task.parallel_for pool ~start:0 ~finish:(nbc-1) ~body:(fun i -> 
   let rec loop(j) =
    if j = nbl then () else (
    let cell = read_cell(src,i,j,nbl) in
    let alive(args) = 
      alive_with(read_cell,args) in 
    let sum = sum_neighborhood
      (alive,src,i,j,nbc,nbl) in
    let new_cell = 
     (cell && sum = 2) || (sum = 3) in
    set_cell(dst,i,j,nbl,new_cell);
    loop(j+1)
   ) in loop(0));
 copy(dst,src) ;;


let d = 1024 ;;
let w = Array.make (d*d) false;;
let w_dst = Array.make (d*d) false ;;

let main () =
  
  w.(0) <- true; (* initial configuration *)
  w.(2) <- true;
  w.(d+1) <- true;
  w.(d+2) <- true;
  w.(d+d+1) <- true;

  for i = 1 to 100 do
     (* print_world(w,d); *)
    array_life(w,w_dst,d,d);
     (* print_world(w_dst,d); *)
    array_life(w_dst,w,d,d);
  done ;;

let c = Task.run pool (fun () ->
  main ());;

