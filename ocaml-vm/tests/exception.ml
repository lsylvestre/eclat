open CustomStdlib ;;

exception E;;

print_int (try 1 + 2 with e -> 42) ;;