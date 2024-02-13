open CustomStdlib ;;


external f : int -> int -> int = "foo" ;;


print_int (f 1 2);;

(*
a.(0) <- 42;;

print (a.(0));;*)
