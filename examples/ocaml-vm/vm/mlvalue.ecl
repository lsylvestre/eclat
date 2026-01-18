

(** representation of values *)

type is_int = bool ;;

type value = long * is_int ;;


let int_of_bool b =
  if b then 1 else 0 ;;

(* constants *)
let val_unit : value = (1,true) ;;
let val_true : value = (1,false) ;;
let val_false : value = (0,false) ;;

(* conversions *)

let long_val (n,_) = n ;;
let ptr_val (n,_) = as_short(n) ;;

let is_int (_,b) = b ;;
let is_long = is_int ;;
let is_ptr (_,b) = not b ;;

let val_int (n:long) : value = (n,true) ;;
let val_long n = val_int(n);; (* synonym *)

let val_ptr (n:short) : value = (as_long(n),false) ;;



let no_scan_tag : char = 251 ;;
let string_tag : char = 252 ;;
let closure_tag : char = 247 ;;
let infix_tag : char = 249 ;;
let fwd_ptr_tag : char = 248 ;;


let make_header ((tag,sz) : (char * short)) : long =
  (long_of_char(tag) lsl 24) lor (as_long(sz) lsl 2) ;;
