
let sum_neighborhood(f,i,n) =
  f(i-n-1) + f(i-n) + f(i-n+1) + 
  f(i-1)            + f(i+1) +
  f(i+n-1) + f(i+n) + f(i+n+1) ;;
  
let next_cell(get_cell,w,i,cell,n) =
  let alive_int(i) = 
    if get_cell(w,i) then 1 else 0 in
  let s : int<4> = sum_neighborhood(alive_int,i,n)
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


(** ==== measure execution time ==== *)

let counter () =
  reg (fun c -> c + 1) last 0 ;;

(**
   $ ./eclat ../benchs/game-of-life/v1/v1.ecl  -main=chrono_main
   $ make simul NS=4000000
     ~> execution time = 21506 cycles
*)
let chrono_main () =
  let cy = counter () in
  let ((),rdy) =
    exec
      let n = 64 in
      let w_src = create 1024 in
      (** no initiation (which would take time), 
          this does not change the timing behavior. *)
      let w_dst = create (length w_src) in
      array_life(w_src,w_dst,n)
    default ()
  in
  if rdy then (print_string "execution time = "; 
               print_int cy; 
               print_string " cycles"; 
               print_newline ()) ;;

(** ==== display successive generations of the world ==== *)

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

(** 
    $ ./eclat -relax ../benchs/game-of-life/v1/v1.ecl  -main=test_main
 *)
let test_main () =
  let n = 8 in
  let w_src = create 50000 in
  let w_dst = create (length w_src) in
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
      array_life(w_src,w_dst,n);
      copy(w_dst,w_src);
      loop(i-1)
    )
  in 
  loop(2000);

  if get(w_src,0) then 0 else 1 ;;

(** ==== synthesis ==== *)

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
