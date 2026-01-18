let read_cell(src, i, j) =
  let v = vect_nth(src, i) in
  let x = vect_nth(v,j) in
  x ;;

let cell_copy_with(src, i, j,y) =
  let v = vect_nth(src, i) in
  let v2 = vect_copy_with(v,j,y) in
  let x = vect_copy_with(src,i,v2) in
  x ;;


let border(i,nb) =
  if i = (-1) then nb - 1 else
  if i = nb then 0 else i ;;

let alive(g,i,j,nbc,nbl) = 
  let cell : bool = read_cell(g, border(i,nbc), border(j,nbl)) in
  if cell then 1 else 0 ;;

let sum_neighborhood(g,i,j,nbc,nbl) =
  alive(g,i-1,j-1,nbc,nbl) + alive(g,i,j-1,nbc,nbl) + alive(g,i+1,j-1,nbc,nbl) +
  alive(g,i-1,j,nbc,nbl)   +                          alive(g,i+1,j,nbc,nbl) +
  alive(g,i-1,j+1,nbc,nbl) + alive(g,i,j+1,nbc,nbl) + alive(g,i+1,j+1,nbc,nbl) ;;

let vect_life (grid,nbc,nbl) =
  let next_line(i,line) = 
    let next_cell(j,cell) = 
      let sum : int<4> = sum_neighborhood(grid,i,j,nbc,nbl) in
      let new_cell = (cell & sum = 2) or (sum = 3) in
      new_cell
    in
    vect_mapi(next_cell,line) in
  vect_mapi(next_line,grid) ;;


(** ==== measure execution time ==== *)

let counter () =
  reg (fun c -> c + 1) last 0 ;;

(**
   $ ./eclat ../benchs/game-of-life/v2/v2.ecl  -main=chrono_main
   $ make simul NAME=chrono_main
     ~> execution time = 1 cycles
*)
let chrono_main () =
  let cy = counter () in
  let ((),rdy) =
    exec
      let a = vect_create<64>(false) in
      let w = vect_create<64>(a) in
      let _ = vect_life(w,64,64) in
      ()
    default ()
  in
  if rdy then (print_string "execution time = "; 
               print_int cy; 
               print_string " cycles"; 
               print_newline ()) ;;

(** ==== display successive generations of the world ==== *)

let print_world (world,nbc,nbl) : unit =
  let _ = vect_mapi ((fun (_,line) ->
     let _ = vect_mapi ((fun (_,cell) ->
       print_string (if cell then "*" else "-")),line) in
       print_newline ()),world) in
  print_string "==============";
  print_newline () ;;


(** 
    $ ./eclat -relax ../benchs/game-of-life/v2/v2.ecl  -main=test_main
 *)
let test_main () =
  let a = vect_create<10>(false) in
  let w0 = vect_create<10>(a) in
  let w0 = cell_copy_with(w0,0,0,true) in
  let w0 = cell_copy_with(w0,0,2,true) in
  let w0 = cell_copy_with(w0,1,1,true) in
  let w0 = cell_copy_with(w0,1,2,true) in
  let w0 = cell_copy_with(w0,2,1,true) in
  let w1 = reg (fun w -> vect_life(w,10,10)) init w0 in
  print_world(w1,10,10);

  (*let f = (fun (i,_) ->
    i = 0 or 
    i = 2 or 
    i = (n+1) or
    i = (n+2) or 
    i = (n+n+1))  *)

  if vect_nth(vect_nth(w1,0),0) then 0 else 1 ;;


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
