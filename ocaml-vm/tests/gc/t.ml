
let rec loop i =
   if i < 0 then () else 
   let x = 42 in
   let (z,_) = 
     let y = (x,x) 
     in y 
   in
   print_int z;
   loop(i-1)
in 
loop (100) ;;
