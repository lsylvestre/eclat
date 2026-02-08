(** custom word/half/char sizes *)

type long = int ;;
type short = int ;;
type byte = int ;;

let as_long (n:short) : long = (n) ;;

let short_of_char (n:byte) : short = (n) ;;
let as_short (n:long) : short = (n) land (1 lsl 16 - 1) ;;

let byte_of_long (n:long) : byte = (n) ;;
let long_of_byte (n:byte) : long = (n) ;;
let byte_of_short (n:short) : byte = (n) land (1 lsl 8 - 1) ;;
let short_of_byte (n:byte) : short = (n) ;;
