let border(i,n) =
  if i = (-1) then n - 1 else
  if i = n then 0 else i ;;

let alive(l,i) = 
  if vect_nth(l,border(i,vect_size l)) then 1 else 0 ;;

let sum_neighborhood(l0,l1,l2,i) =
  alive(l0,i-1) + alive(l1,i-1) + alive(l2,i-1) + 
  alive(l0,i)                   + alive(l2,i) +
  alive(l0,i+1) + alive(l1,i+1) + alive(l2,i+1) ;;

let next_cell_with_lines(i,line0,line1,line2) =
  let cell = vect_nth(line1,i) in
  let sum : int<4> = sum_neighborhood(line0,line1,line2,i) in
  (cell & sum = 2) or (sum = 3) ;;

let vect_array_life (world : bool vect<'a> array<'b>) : unit = 
  let first_line = get(world,0) in
  let rec aux (line0,line1,i) : unit =
    if i < length(world) then
      (let line2 = if i = length(world) - 1 
                   then first_line 
                   else get(world,i+1)
       in
       let next (j,cell) = 
         next_cell_with_lines(j,line0,line1,line2)
       in
       set(world,i,(vect_mapi(next,line1)));
       aux(line1,line2,i+1))
    else ()
in aux(get(world,length(world)-1),first_line,0) ;;


(** ==== measure execution time ==== *)

let counter () =
  reg (fun c -> c + 1) last 0 ;;

(**
   $ ./eclat ../benchs/game-of-life/v3/v3.ecl  -main=chrono_main
   $ make simul NAME=chrono_main
*)
let chrono_main () =
  let cy = counter () in
  let ((),rdy) =
    exec
      let n = 8 in
      let w0 : bool vect<8> = vect_create<8>(false) in
      let w : bool vect<8> array<8> = create<8>() in
      set(w,0,w0);
      (** no initiation (which would take time), 
          this does not change the timing behavior. *)
      vect_array_life(w)
    default ()
  in
  if rdy then (print_string "execution time = "; 
               print_int cy; 
               print_string " cycles"; 
               print_newline ()) ;;

(** ==== display successive generations of the world ==== *)

let print_world (world) : unit =
  for i = 0 to length(world) - 1 do
    let line_i = get(world,i) in
    for j = 0 to vect_size(line_i) - 1 do
      print_string (if vect_nth(line_i,j) then "*" else "-")
    done;
    print_newline ()
  done;
  print_string "==============";
  print_newline () ;;


(** 
    $ ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -main=test_main
    $  make simul NAME=test_main NS=400000
 *)
let test_main () =
  let w0 = vect_create<8>(false) in
  let w = create<8>() in
  for i = 0 to length w - 1 do
    let wi = if i = 0 then vect_copy_with(vect_copy_with(w0,0,true),2,true) else
             if i = 1 then vect_copy_with(vect_copy_with(w0,1,true),2,true) else
             if i = 2 then vect_copy_with(w0,1,true) else w0 in
    set(w,i,wi)
  done;

  let rec loop(i) =
    if i < 1 then () else (
      print_world(w);
      vect_array_life(w);
      loop(i-1)
    )
  in 
  loop(2000);

  if vect_nth(get(w,0),0) then 0 else 1 ;;



(** ==== synthesis ==== *)

(** ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -intel-max10  -main=main_intel *)

let main_intel (_:int<12>) : int<58> =
  let v = test_main() in
  resize_int<58>(v) ;;

(** ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -xilinx-zybo  -main=main_xilinx *)

let main_xilinx (i:int<8>) : int<4> =
  let v = test_main() in
  resize_int<4>(v) ;;

(** ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -yosys-ecp5  -main=main_yosys *)

let main_yosys (i:int<1>) : int<1> =
  let v = test_main() in
  resize_int<1>(v) ;;

