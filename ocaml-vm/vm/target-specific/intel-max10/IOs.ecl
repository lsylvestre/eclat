

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

let all_leds (b:bool) : leds =
  (b,b,b,b,b,b,b,b,b,b) ;;

(* for displaying "COFFEE" on the six 7-segment displays *)
let display_coffee : t_7segs =
  (displayC, displayO, displayF, displayF, displayE, displayE) ;;


(* for displaying "000000" on the six 7-segment displays *)
let display_zero : t_7segs =
  (display0, display0, display0, display0, display0, display0) ;;



let map_digit(n: int<4>) =
    match n with
    | 0 -> display0
    | 1 -> display1
    | 2 -> display2
    | 3 -> display3
    | 4 -> display4
    | 5 -> display5
    | 6 -> display6
    | 7 -> display7
    | 8 -> display8
    | 9 -> display9
    | 10 -> displayA
    | 11 -> displayB
    | 12 -> displayC
    | 13 -> displayD
    | 14 -> displayE
    | 15 -> displayF
    | _ -> displayL
    end ;;

let number_to_alpha(n : int<25>) = (* 25 car signe *)
    let i1 = int_of_tuple<4>(get_bit(n, 3), get_bit(n, 2), get_bit(n, 1), get_bit(n, 0)) in
    let i2 = int_of_tuple<4>(get_bit(n, 7), get_bit(n, 6), get_bit(n, 5), get_bit(n, 4)) in
    let i3 = int_of_tuple<4>(get_bit(n, 11), get_bit(n, 10), get_bit(n, 9), get_bit(n, 8)) in
    let i4 = int_of_tuple<4>(get_bit(n, 15), get_bit(n, 14), get_bit(n, 13), get_bit(n, 12)) in
    let i5 = int_of_tuple<4>(get_bit(n, 19), get_bit(n, 18), get_bit(n, 17), get_bit(n, 16)) in
    let i6 = int_of_tuple<4>(get_bit(n, 23), get_bit(n, 22), get_bit(n, 21), get_bit(n, 20)) in
    (map_digit(i1), map_digit(i2), map_digit(i3), map_digit(i4), map_digit(i5), map_digit(i6)) ;;

