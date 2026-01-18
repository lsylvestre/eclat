(** custom word/half/char sizes *)

type long = int ;;
type short = int ;;
type char = int ;;

let as_long (n:short) : long = (n) ;;

let short_of_char (n:char) : short = (n) ;;
let as_short (n:long) : short = (n) land (1 lsl 16 - 1) ;;

let char_of_long (n:long) : char = (n) ;;
let long_of_char (n:char) : long = (n) ;;
let char_of_short (n:short) : char = (n) land (1 lsl 8 - 1) ;;
let short_of_char (n:char) : short = (n) ;;
