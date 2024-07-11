
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

let vect_life (world,n) =
  let access(w,i) = vect_nth(w,pos_modulo(i,n,vect_size w)) in  
  let f (i,cell) = next_cell(access,world,i,cell,n) in
  vect_mapi(f,world) ;;


(** ==== measure execution time ==== *)

let counter () =
  reg (fun c -> c + 1) last 0 ;;

(**
   $ ./eclat ../benchs/game-of-life/v2/v2.ecl  -main=chrono_main
   $ make simul
     ~> execution time = 1 cycles
*)
let chrono_main () =
  let cy = counter () in
  let ((),rdy) =
    exec
      let n = 64 in
      let w = vect_create(1024,false) in
      let _ = vect_life(w,n) in
      ()
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
    print_string (if vect_nth(world,i) then "*" else "-")
  done;
  print_newline ();
  print_string "==============";
  print_newline () ;;

(** 
    $ ./eclat -relax ../benchs/game-of-life/v2/v2.ecl  -main=test_main
 *)
let test_main () =
  let n = 70 in
  let w = vect_make(size_create 4900,false) in

  let f = (fun (i,_) ->
    i = 0 or 
    i = 2 or 
    i = (n+1) or
    i = (n+2) or 
    i = (n+n+1))
  in

  let w = vect_mapi(f,w) in

  let rec loop(i,w) =
    if i < 1 then w else (
      print_world(w,n);
      let next_w = vect_life(w,n) in
      loop(i-1,next_w)
    )
  in 
  let w_end = loop(2000,w) in

  if vect_nth(w_end,0) then 0 else 1 ;;

(** ==== synthesis ==== *)

(** ./eclat -relax ../benchs/game-of-life/v2/v2.ecl  -intel-max10  -main=main_intel *)

let main_intel (_:int<12>) : int<58> =
  let v = test_main() in
  resize_int<58>(v) ;;

(** ./eclat -relax ../benchs/game-of-life/v2/v2.ecl  -xilinx-zybo  -main=main_xilinx *)

let main_xilinx (i:int<8>) : int<4> =
  let v = test_main() in
  resize_int<4>(v) ;;

(** ./eclat -relax ../benchs/game-of-life/v2/v2.ecl  -yosys-ecp5  -main=main_yosys *)

let main_yosys (i:int<1>) : int<1> =
  let v = test_main() in
  resize_int<1>(v) ;;
