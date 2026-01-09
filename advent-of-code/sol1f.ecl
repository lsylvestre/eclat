(*********************************************************************)
(*                                                                   *)
(*                     Advent Of Code 2025                           *)
(*                                                                   *)
(*           puzzle 1 - task 1 & 2 - Eclat version                   *)
(*                                                                   *)
(*********************************************************************)
(*                                                                   *)
(* author : Emmanuel Chailloux                                       *)
(*                                                                   *)
(*********************************************************************)

(* type of commands for rotations *)
type command = R of int<10> | L of int<10> ;;

(* contents of the file “input0.txt”  

L68
L30
R48
L5
R60
L55
L1
L99
R14
L82

*)

let input0 = create<10> () ;;
let init_input0 () = 
  set(input0,0,L 68) ;
  set(input0,1,L 30) ; 
  set(input0,2,R 48 ) ;
  set(input0,3,L 5) ;
  set(input0,4,R 60) ;
  set(input0,5,L 55) ;
  set(input0,6,L 1) ;
  set(input0,7,L 99) ;
  set(input0,8,R 14) ;
  set(input0,9,L 82)  
;;

(* global variables *)

let debug = false ;;

(* original ocaml code 

let global_zero = ref 0 ;;
let global_all_zero = ref 0 ;;

let global_command = ref 0 ;;
let global_pos = ref 50 ;;
*)

let global_zero = create<1> () ;;
let global_all_zero =  create<1> () ;;

let global_command = create<1> () ;;
let global_pos =  create<1> () ;;

let init_global_vars () = 
   set(global_zero,0,0);
   set(global_all_zero,0,0);
   set(global_command,0,0);
   set(global_pos,0,50)
;;

let incr_step (a,s) = set(a,0,get(a,0)+s) ;;
let incr(a) = incr_step(a,1) ;;

(* naive version of integer division - need to speed it up *)

let rec division_entiere_aux (a,b,q) = 
  if a < b then (q,a)
  else division_entiere_aux ((a-b), b, (q+1)) 
;;

let division_entiere (a, b) = division_entiere_aux (a, b, 0) ;;

(*
 *
 *  2 specialized functions for right and left  rotations
 *
 *)
    
let rec one_step_right step_mc = 
  let m = get(global_pos,0) + step_mc in 
     if m >= 100 then 
       ( incr global_all_zero ;
         set(global_pos, 0, m - 100) 
       ) 
      else
        ( (if m == 0 then  incr global_all_zero ) ; 
          set(global_pos,0,m)        
        )
;;


let rec one_step_left step_mc =
  let m = get(global_pos,0) - step_mc in 
  if m < 0 then ( 
     (if (get(global_pos,0) != 0) then incr global_all_zero) ;
     set(global_pos,0, 100 + m) 
    )
  else 
    ( ( if m == 0 then incr global_all_zero  ) ; 
       set(global_pos,0,m)
    )
;;

(* the main function of a rotation *)

let one_step (command) = 
  let step = 
    (match command with R(s) -> s | L(s) -> s) 
  in
  let (q,r) = division_entiere(step, 100)  in 
  incr_step(global_all_zero,q) ;
  ( match command with 
     | R(step) -> one_step_right step
     | L(step) -> one_step_left step
  ) ;
  ( if (get(global_pos,0)  == 0) then incr(global_zero)  ) ;
  incr(global_command)
;;

(* 
 *
 * main function : executes all commands from input0 
 *  and displays the results: v1 (task 1) and v2 (task 2)
 *
 *)

let main () = 
  exec
  init_input0();
  init_global_vars();
  let rec aux(i,n) = 
    if i>= n then () 
    else (
      one_step(get(input0,i));
      aux(i+1,n)
    )
  in 
    aux(0,10);
    if debug then (
	    print_int (get(global_command,0)); print_string " : pos = "; 
      print_int (get(global_pos,0));
	    print_string " ; zero = "; print_int (get(global_zero,0));
	    print_string " ; all_0 = "; print_int (get(global_all_zero,0)); print_newline()
    ) ;
  print_string "nb commandes : "; print_int (get(global_command,0)) ; print_newline() ;
  print_string "pos finale : "; print_int (get(global_pos,0)) ; print_newline() ; 
  print_string "nb 0 v1 : " ; print_int (get(global_zero,0)) ; print_newline() ;
  print_string "nb 0 v2 : " ; print_int (get(global_all_zero,0)) ; print_newline()
  default ()
;;
 
main() ;;
 
(*
% make NS=2000
ghdl -a  runtime.vhdl stdlib.vhdl 
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=2000ns
nb commandes : 10 
pos finale : 32 
nb 0 v1 : 3 
nb 0 v2 : 6 
./tb_main:info: simulation stopped by --stop-time @2us
*)    