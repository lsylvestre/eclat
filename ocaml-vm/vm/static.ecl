let static stack = (0,true)^4096 ;;
let static ram = (0,true)^32768 ;;

let static global_end = 0^1 ;;

let static from_space_array = (0:short)^1 ;;
let static to_space_array = (0:short)^1 ;;
let static next_array = (0:short)^1 ;;

let static caml_fresh = (0:long)^1 ;;

