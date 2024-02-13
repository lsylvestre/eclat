open CustomStdlib ;;

type t = A of t * t | B of int ;;

print_int (match A(B 1, B 2) with A(B _,_) -> 42 | B(n) -> n | _ -> 200);;
