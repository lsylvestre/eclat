(*********************************************************************)
(*                                                                   *)
(*                     Advent Of Code 2025                           *)
(*                                                                   *)
(*           puzzle 1 - task 1 & 2 in Eclat - version 2              *)
(*                                                                   *)
(*********************************************************************)
(*                                                                   *)
(* author : Loïc Sylvestre                                           *)
(*                                                                   *)
(*********************************************************************)


(* $ ./eclat ../j1/v2/sol1f_ex_v2.ecl -arg="(L(68),false);(L(30),false);(R(48),false);(L(5),false);(R(60),false);(L(55),false);(L(1),false);(L(99),false);(R(14),false);(L(82),false);(L(0),true)" *)

let debug = true ;;

type command = L of int<12> | R of int<12> ;;

let one_step ((is_left, step), all_zero, pos, pos_is_zero) =
  let m = pos + (if is_left then - step else step) in 
  let azp1 = all_zero + 1 in
  let test = if is_left then (m < 0) else (m >= 100) in
  if test then
     let az = if is_left & pos_is_zero then all_zero else azp1 in
     let next_pos = m + (if is_left then 100 else (- 100)) in
     (az, next_pos)
  else let az = if m = 0 then azp1 else all_zero in 
       let next_pos = m in
       (az, next_pos) ;;

let decode_cmd (cmd:command) : (bool * int<12>) = 
  match cmd with 
  | L(step) -> (true, step) 
  | R(step) -> (false, step) ;;

let exec_cmd ((is_left,step), (zeros, all_zero, pos, pos_is_zero)) =
    let q = step / 100 in
    let all_zero = all_zero + q in
    let (next_all_zero, next_pos) = one_step((is_left,step), all_zero, pos, pos_is_zero) in
    let next_pos_is_zero = (next_pos = 0) in
    let next_zeros = if next_pos_is_zero then zeros + 1 else zeros in
    if debug then (
       print_string "pos: " ; print_int next_pos; print_newline ();
       print_string "task 1: " ; print_int next_zeros; print_newline ();
       print_string "task 2: " ; print_int next_all_zero; print_newline ();
       print_string "---------------------"; print_newline ()
    );
    (next_zeros, next_all_zero, next_pos, next_pos_is_zero) ;;

let main (cmd,finished) =
  reg (fun s -> 
         if finished then s else exec_cmd(decode_cmd(cmd),s))
  init (0,0,50,false) ;;

(******************************************************
 implementation on an Intel MAX 10 FPGA with physical I/Os :
   - n_sw : 10 switch buttons
   - button1, button2 : 2 press buttons 
   - leds_0_to_4, leds_5_to_9 : 10 LEDs 
*********************************************************)

(* $ ./eclat -main=main_max10_fpga -intel-max10 ../j1/v2/sol1f_ex_v2.ecl *)

let main_max10_fpga ((n_sw,button1,button2) : int<10> * bool * bool) : (int<5> * int<5> * int<48>)  =
  let step = resize_int<12>(n_sw) in
  let cmd = if button1 then L(step) else R(step) in
  let finished = button2 in
  let (all_zero,zeros,_,_) = main (cmd,finished) in
  let leds_0_to_4 = resize_int<5>(zeros) in
  let leds_5_to_9 = resize_int<5>(all_zero) in
  (leds_0_to_4, leds_5_to_9, 0) ;;


