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
type command = R of int<12> | L of int<12> ;;


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

(* 
let division_entiere (a, b) = division_entiere_aux (a, b, 0) ;;
*)

 
let division_entiere (a, b)  = 
   let q  = a / b in  
    (q, a-b*q) ;;


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
    (match command with 
        R(s) -> s
      | L(s) -> s
    ) 
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
 * resolve function : executes all commands from input0 
 *  and displays the results: v1 (task 1) and v2 (task 2)
 *
 *)

 
let dim_command = 10 ;;
(* let dim_command = 4531 ;; *)
(* let dim_command = 10 ;; *)
let input : command array<'N> = create<10> () ;;
(* let input : command array<'N> = create<10> () ;; *)
(* let input : command array<'N> = create<4531> () ;; *)
let dim_char = 38  ;;
(* let dim_char = 38 ;; *)
(* let dim_char = 1867 ;; *)

let print_command(c) = match c with 
   R i -> print_string "R"; print_int i ; print_newline()
 | L i -> print_string "L"; print_int i ; print_newline()
 ;;

 let print_input () = 
  print_string "input : "; print_newline();
  for i = 0 to dim_command - 1 do 
    print_command (get(input,i))
  done ; 
  ()
;;

let resolve() =
  for i = 0 to dim_command - 1  do 
    one_step(get(input,i))
  done ;
  () 
;;

let print_results () = 
  print_string "nb commands : "; 
  print_int (get(global_command,0)) ; 
  print_newline() ;
  print_string "final position : "; 
  print_int (get(global_pos,0)) ; 
  print_newline() ; 
  print_string "nb 0 (task 1) : " ; 
  print_int (get(global_zero,0)) ; 
  print_newline() ;
  print_string "nb 0 (task 2) : " ; 
  print_int (get(global_all_zero,0)) ; 
  print_newline()
;;



(* reading "input.txt" file *)

(* an example ontents with file “input0.txt” : 

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



let int_of_bytes (by,i) =
  let rec aux(n) = 
    let token_digit = 
      resize_int<12>(char_code(bytes_get(by,get(i,0)))) - 48 
    in 
      incr i;
      if ((token_digit < 0) or (token_digit > 9)) then n 
      else aux(10*n + token_digit) 
  in
    aux(0)
;; 


let next_token(by,i) =
  if debug then (
    print_string "pos : ";print_int (get(i,0)) ; print_string ".. "
  ) ;
  let token_dir  = char_code(bytes_get(by,get(i,0))) in 
  if debug then (print_int token_dir; print_string "..");
  incr i;
  let token_int = int_of_bytes (by,i)  in
    if debug then (print_string "$"; print_int (get(i,0)); print_string "$"; 
      print_int token_int; 
      print_command (R(token_int)); print_string "$"; print_newline()); 
    if token_dir = 76 then (L token_int) else (R token_int)
;;

let init_input (by) = 
  let i = create<1> () in
  let j = create<1> () in  
    set(i,0,0);
    set(j,0,0);
    let rec aux() = 
      if get(j,0) < dim_command then (
        let t = next_token (by,i) in 
          set(input,get(j,0),t);
          incr j ;
          aux () 
      )
      else (if debug then (print_string "end of input initialization"; print_newline()))
    in aux () 
;;

(*
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
*)

(* cycle counter *)
let chrono () = 
  reg (fun c -> c + 1) init (-1) ;;


let main () = 
  let cy = chrono() in  
    reg (fun ((),rdy) ->
      if rdy then
        let _ = 
          exec 
            print_string "execution time (nb cycles): ";
            print_int cy;
            print_newline ();
            let rec fini() = fini() 
            in fini()
          default ()
        in ((),true)
      else
        exec
          print_string "reading the file `input.txt`";
          print_newline ();
            let by = (input_file "puzzle1-input0.txt" : bytes<38> (* <*1868> <38> *)) in
              if debug then (bytes_print by ; print_newline());
              init_input (by);
              print_input();
              init_global_vars();
              resolve(); 
              print_results()
        default ()) 
    init ((),false) 
;;

(*
main() ;;
 **)
(* recent 

% cd ../eclat-compiler 
% ./eclat     ../advent-of-code/extra.ecl ../advent-of-code/sol1f_ex.ecl

vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.
emmanuel@Santeuil eclat-compiler % cd ../target 
emmanuel@Santeuil target %   make EXTERNALS=../advent-of-code/extra.vhdl NS=500000000                                                        

ghdl -a  runtime.vhdl stdlib.vhdl ../advent-of-code/extra.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=500000000ns
reading the file `input.txt` 
input :  
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
nb commands : 10 
final position : 32 
nb 0 (task 1) : 3 
nb 0 (task 2) : 6 
execution time (nb cycles): 359 


old1

% ghdl -a  runtime.vhdl stdlib.vhdl ../advent-of-code/extra.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=500000000ns
reading the file `input.txt` 
input :  
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
nb commands : 10 
final position : 32 
nb 0 (task 1) : 3 
nb 0 (task 2) : 6 
execution time (nb cycles): 369 

old 2
make NS=2000
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