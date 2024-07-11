

(* physical I/Os available on the DE-10 lite FPGA board

  inputs:
  - 10 x switchs
  - 2 x keys (buttons)

  outputs:
  - 10 x leds
  - 6 x 7-segment displays

*)

type led = bool ;;
type switch = bool ;;
type key = bool ;;

(* 7 segment display (for the De10-lite board) *)

(* type of value to be printed on a 7 segment displayer.
   It comrpises 8 boolean values :
     - one per segment
     - one for displaying a point at the bottom left
       of the displayer: "0.", "1.", "2." ... *)
type t_7seg = (bool * bool * bool * bool * bool * bool * bool * bool) ;;

type switches = (switch * switch * switch * switch * switch *
                 switch * switch * switch * switch * switch) ;;

type keys = (key * key) ;;

type leds = (led * led * led * led * led * led * led * led * led * led) ;;

type t_7segs = (t_7seg * t_7seg * t_7seg * t_7seg * t_7seg * t_7seg) ;;


type inputs = switches * keys ;;

type outputs = leds * t_7segs ;;


let display0 : t_7seg = tuple_of_int<8>(0b00000011) ;;
let display1 : t_7seg = tuple_of_int<8>(0b10011111) ;;
let display2 : t_7seg = tuple_of_int<8>(0b00100101) ;;
let display3 : t_7seg = tuple_of_int<8>(0b00001101) ;;
let display4 : t_7seg = tuple_of_int<8>(0b10011001) ;;
let display5 : t_7seg = tuple_of_int<8>(0b01001001) ;;
let display6 : t_7seg = tuple_of_int<8>(0b01000001) ;;
let display7 : t_7seg = tuple_of_int<8>(0b00011111) ;;
let display8 : t_7seg = tuple_of_int<8>(0b00000001) ;;
let display9 : t_7seg = tuple_of_int<8>(0b00001001) ;;
let displayA : t_7seg = tuple_of_int<8>(0b00010001) ;;
let displayB : t_7seg = tuple_of_int<8>(0b11000001) ;;
let displayC : t_7seg = tuple_of_int<8>(0b01100011) ;;
let displayD : t_7seg = tuple_of_int<8>(0b10000101) ;;
let displayE : t_7seg = tuple_of_int<8>(0b01100001) ;;
let displayF : t_7seg = tuple_of_int<8>(0b01110001) ;;
let displayG : t_7seg = tuple_of_int<8>(0b00001001) ;;
let displayH : t_7seg = tuple_of_int<8>(0b10010001) ;;
let displayI : t_7seg = tuple_of_int<8>(0b10011111) ;;
let displayJ : t_7seg = tuple_of_int<8>(0b10001111) ;;
let displayK : t_7seg = tuple_of_int<8>(0b01010001) ;;

let displayL : t_7seg = tuple_of_int<8>(0b11100011) ;;
let displayN : t_7seg = tuple_of_int<8>(0b11010101) ;;
let displayO : t_7seg = tuple_of_int<8>(0b00000011) ;;
let displayP : t_7seg = tuple_of_int<8>(0b00110001) ;;
let displayQ : t_7seg = tuple_of_int<8>(0b00011001) ;;
let displayR : t_7seg = tuple_of_int<8>(0b11110101) ;;
let displayS : t_7seg = tuple_of_int<8>(0b01001001) ;;
let displayT : t_7seg = tuple_of_int<8>(0b11100001) ;;
let displayU : t_7seg = tuple_of_int<8>(0b10000011) ;;
let displayV : t_7seg = tuple_of_int<8>(0b11000111) ;;
let displayX : t_7seg = tuple_of_int<8>(0b11011001) ;;
let displayY : t_7seg = tuple_of_int<8>(0b10001001) ;;
let displayZ : t_7seg = tuple_of_int<8>(0b00100101) ;;

let all_switches (b:bool) : switches =
  (b,b,b,b,b,b,b,b,b,b) ;;

(* for displaying "COFFEE" on the six 7-segment displays *)
let display_coffee : t_7segs =
  (displayC, displayO, displayF, displayF, displayE, displayE) ;;


(* for displaying "000000" on the six 7-segment displays *)
let display_1234567 : t_7segs =
  (display1, display2, display3, display4, display5, display6) ;;


(* for displaying "000000" on the six 7-segment displays *)
let display_zero : t_7segs =
  (display0, display0, display0, display0, display0, display0) ;;







(* [counter (rst)] is instantaneous,
   it increments its output each time it is re-executed *)
let counter (rst) =
  let inc(c) =
    if rst then 0 else c + 1
  in
  reg inc last 0 ;;


let a = create 100 ;;

(* blinking a led each n clock cycles *)
let blink (n) =
  let inc c = if c = n + n then 0 else c + 1 in
  (reg inc last 0) > n
;;


(* see in IOs.ecl definitions of types inputs and outputs *)

let main (ii : inputs) : outputs =
  let (_,(k1,k2)) = ii in
  if k1 then (all_switches false,display_zero) else
  (* blinking a led *)
  let (_,b) = reg (fun (pre_v,b) ->
        let (v,rdy) = exec 
          let v = get(a,0) in
          set(a,0,pre_v+1);
          for u = 0 to pre_v*10000000 do
            ()
          done; v
        default 1
        in if rdy then (v,not b) else (pre_v,b))
   last (0,false)
  in
  let switches = all_switches b in
  let dspl = if b then display_coffee else display_1234567 in
  (switches, dspl) ;;
