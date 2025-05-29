(** custom word/half/char sizes *)

type long = int<31> ;;
type short = int<16> ;;
type char = int<8> ;;

let as_long n : long = resize_int<31>(n) ;;


let short_of_char (n:char) : short = resize_int<16>(n) ;;
let as_short (n:long) : short = resize_int<16>(n) ;;

let char_of_long (n:long) : char = resize_int<8>(n) ;;
let long_of_char (n:char) : long = resize_int<31>(n) ;;
let char_of_short (n:short) : char = resize_int<8>(n) ;;
let short_of_char (n:char) : short = resize_int<16>(n) ;;
