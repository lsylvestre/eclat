(** custom word/half/char sizes *)

type long = int<32> ;;
type short = int<16> ;;
type byte = int<8> ;;

let as_long n : long = resize_int<32>(n) ;;


let short_of_byte (n:byte) : short = resize_int<16>(n) ;;
let as_short (n:long) : short = resize_int<16>(n) ;;

let byte_of_long (n:long) : byte = resize_int<8>(n) ;;
let long_of_byte (n:byte) : long = resize_int<32>(n) ;;
let byte_of_short (n:short) : byte = resize_int<8>(n) ;;
let short_of_byte (n:byte) : short = resize_int<16>(n) ;;

type comparaison =
    Eq of unit
  | Lt of unit
  | Neq of unit
  | Ge of unit
  | Le of unit
  | Gt of unit ;;


