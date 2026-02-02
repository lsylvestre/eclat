
let f <<?p>> (x) = 
  parfor i = 0 to ?p do 
    print_int i
  done ;;

let main () = f<<10>>(42);;