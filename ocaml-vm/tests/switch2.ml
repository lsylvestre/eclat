open CustomStdlib ;;

type t = A | B of int | C ;;

print_int (match B 43 with A -> 42 | B(n) -> n | C -> 44);;
