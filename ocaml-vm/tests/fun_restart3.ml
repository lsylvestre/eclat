open CustomStdlib ;;


let c = 100 ;;

let f x y z = x + y + z + c ;;

let a = 1;;

let g = f a;;

let h = g 5;;

print_int (h 42) ;;