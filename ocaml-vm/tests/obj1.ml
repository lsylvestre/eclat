open CustomStdlib ;;


let x = object method f = 42 end  ;;


print_int (x#f);;

(*
a.(0) <- 42;;

print (a.(0));;*)
