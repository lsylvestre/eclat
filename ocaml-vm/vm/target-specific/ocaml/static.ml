let stack = Array.make 4096 (0,false) ;;
let ram = Array.make 32768 (0,false) ;;

let global_end = Array.make 1 0 ;;

let from_space_array = Array.make 1 0 ;;
let to_space_array = Array.make 1 0 ;;
let next_array = Array.make 1 0 ;;



let caml_fresh = Array.make 1 0 ;;


let pause a = a ;;

let length = Array.length ;;