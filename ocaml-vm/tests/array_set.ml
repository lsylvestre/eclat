open CustomStdlib

let a = [|42;2;3;4;5;6|] ;;

Array.set a 0 200;;

print_int (Array.get a 0);;
(*
a.(0) <- 42;;

print (a.(0));;*)
