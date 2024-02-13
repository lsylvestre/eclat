open CustomStdlib ;;

exception E;;

let f x = if x > 0 then raise E else 30 ;;

print_int (try f 5 with e -> 42) ;;