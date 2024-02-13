open CustomStdlib ;;


(* heap_size = 20 *)

let rec loop i =
   if i < 0 then () else
   let f () x = x - 1 in
   let g = f () in
   loop (g i)
in 
loop (100) ;;
