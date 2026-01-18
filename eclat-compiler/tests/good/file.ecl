
let x : int<15> array<12> = create<12>();;


array_from_file(x,"foo.txt");;

let main () =   
    exec 
      for i = 0 to length(x) - 1 do
        print_int(get(x,i)) 
      done
   default ();;
