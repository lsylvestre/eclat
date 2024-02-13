open CustomStdlib ;;

exception E;;

let f b = if b then raise E else 30 ;;

print_int (try f true with e -> 42) ;;