(* $ ./eclat -relax ../benchs/game-of-life/v1/v1.ecl  -main=test_main 
   $ make simul NS=4000000 NAME=test_main 
*)

let read_cell(src, i, j, nbl) =
  get(src, i + j * nbl) ;;

let set_cell(dst, i, j, nbl,v) =
  set(dst, i + j * nbl,v) ;;

let border(i,nb) =
  if i = (-1) then nb - 1 else
  if i = nb then 0 else i ;;

let alive(g,i,j,nbc,nbl) = 
  let v : bool = read_cell(g, border(i,nbc), border(j,nbl), nbl) in
  if v then 1 else 0 ;;

let sum_neighborhood(g,i,j,nbc,nbl) =
  alive(g,i-1,j-1,nbc,nbl) + alive(g,i,j-1,nbc,nbl) + alive(g,i+1,j-1,nbc,nbl) +
  alive(g,i-1,j,nbc,nbl) +                            alive(g,i+1,j,nbc,nbl) +
  alive(g,i-1,j+1,nbc,nbl) + alive(g,i,j+1,nbc,nbl) + alive(g,i+1,j+1,nbc,nbl) ;;

let array_mapi (f,src,dst) =
 let rec aux(i) =
   if i < length(src) then
   (set(dst,i,f(i,get(src,i))); 
    aux(i+1)) else () 
 in aux(0) ;;

let copy(src,dst) =
  array_mapi((fun (_,x) -> x),src,dst) ;;

let array_life ((src, nbc, nbl) : bool array<'N> * int<16> * int<16>) =
  let dst = create<'N>() in

  let rec loop(i,j) =
     (* print_string "=>"; print_int i; print_string ";"; print_int j; print_newline ();*)
    if i = nbc then () else
    if j = nbl then loop(i+1,0) else
    ( let cell = get(src,i + j * nbl) in
      let s : int<4> = sum_neighborhood(src,i, j,nbc,nbl) in
      let v = (cell & s = 2) or (s = 3) in
      set(dst,i + j * nbl,v);
      loop(i,j+1) )
  in 
  loop(0,0);
  copy(dst,src) ;;



(** ==== display successive generations of the world ==== *)

let print_world (world,n) : unit =
  for i = 0 to n*n - 1 do
    if (i mod n) = 0 then print_newline () else ();
    print_string (if get(world,i) then "*" else "-")
  done;
  print_newline ();
  print_string "==============";
  print_newline () ;;



let test_main () =
  let n = 8 in
  let w_src = create<64> () in
  let f = (fun (i,_) ->
    i = 0 or 
    i = 2 or 
    i = (n+1) or
    i = (n+2) or 
    i = (n+n+1))
  in
  
  (* initial world *)
  array_mapi(f,w_src,w_src);

  let rec loop(i) =
    if i < 1 then () else (
      print_world(w_src,n);
      array_life(w_src,n,n);
      loop(i-1)
    )
  in 
  loop(400);

  if get(w_src,0) then 0 else 1 ;;



(** ./eclat -relax ../benchs/game-of-life/v1/v1.ecl  -intel-max10  -main=main_intel *)

let main_intel (_:int<12>) : int<58> =
  let v = test_main() in
  resize_int<58>(v) ;;

(** ./eclat -relax ../benchs/game-of-life/v1/v1.ecl  -xilinx-zybo  -main=main_xilinx *)

let main_xilinx (i:int<8>) : int<4> =
  let v = test_main() in
  resize_int<4>(v) ;;

(** ./eclat -relax ../benchs/game-of-life/v1/v1.ecl  -yosys-ecp5  -main=main_yosys *)

let main_yosys (i:int<1>) : int<1> =
  let v = test_main() in
  resize_int<1>(v) ;;



(** ==== measure execution time ==== *)

let counter () =
  reg (fun c -> c + 1) last (-1) ;;

(**
   $ ./eclat ../benchs/game-of-life/v1/v1.ecl  -main=chrono_main
   $ make simul NS=4000000
     ~> execution time = 11394 cycles
*)
let chrono_main () =
  let cy = counter () in
  let ((),rdy) =
    exec
      let n = 64 in
      let m = 16 in
      let w_src = create<1024> () in
      (** no initiation (which would take time), 
          this does not change the timing behavior. *)
      array_life(w_src,n,m)
    default ()
  in
  if rdy then (print_string "execution time = "; 
               print_int cy; 
               print_string " cycles";
               print_newline ()) ;;
