
let rec loop (f) = 
  let g x = f x + 1 in 
  loop g;;

let main () =
  let f x = x in
  print_int (loop(f)) ;;
