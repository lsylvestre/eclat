open CustomStdlib ;;

type t = A of bool | B of int ;;

print_int (match A true with A _ -> 42 | B(n) -> n);;
