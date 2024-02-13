open CustomStdlib ;;

type t = A | B | C | D ;;

print_int (match B with A -> 42 | B -> 43 | C -> 44 | D -> 45);;
