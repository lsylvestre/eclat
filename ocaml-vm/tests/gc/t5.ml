open CustomStdlib ;;

let rec loop (i,r) =
   if i < 0 then (let (z,_,_,_,_,_) = r in print_int z)
   else 
   let x = 1 in
   loop(i-1,(x,x,x,x,x,x) )
in 
loop (1000,(0,0,0,0,0,0)) ;;