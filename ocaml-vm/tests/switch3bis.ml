open CustomStdlib ;;

type t = A of bool | B of int ;;

print_int (match B 43 with A _ -> 42 | B(n) -> n);;
