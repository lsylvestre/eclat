open CustomStdlib ;;

exception E;;

let a = 5 ;;

print_int (try if a > 0 then raise E else 30 with e -> 42) ;;