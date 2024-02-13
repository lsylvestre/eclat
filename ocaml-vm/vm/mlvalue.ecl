(** custom word/half/char sizes *)

type long = int<31> ;;
type short = int<16> ;;
type char = int<8> ;;

let as_long (n:short) : long = resize_int<31>(n) ;;

let short_of_char (n:char) : short = resize_int<16>(n) ;;
let as_short (n:long) : short = resize_int<16>(n) ;;

let char_of_long (n:long) : char = resize_int<8>(n) ;;
let long_of_char (n:char) : long = resize_int<31>(n) ;;
let char_of_short (n:short) : char = resize_int<8>(n) ;;
let short_of_char (n:char) : short = resize_int<16>(n) ;;


(** representation of values *)

type is_int = bool ;;

type value = long * is_int ;;


let int_of_bool b =
  if b then 1 else 0 ;;

(* constants *)
let val_unit = (1,true) ;;
let val_true = (1,false) ;;
let val_false = (0,false) ;;

(* conversions *)

let long_val (n,_) = n ;;
let ptr_val (n,_) = as_short(n) ;;

let is_int (_,b) = b ;;
let is_ptr (_,b) = not b ;;

let val_int (n:long) : value = (n,true) ;;
let val_long n = val_int(n);; (* synonym *)

let val_ptr (n:short) : value = (as_long(n),false) ;;

