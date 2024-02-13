open CustomStdlib ;;

type t = A of t * t | B of int ;;

print_int (match A(B 10, B 11) with A _ -> 42 | B(n) -> n | _ -> 200);;
