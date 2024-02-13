open CustomStdlib ;;

exception E;;

print_int (try raise E with e -> 42) ;;