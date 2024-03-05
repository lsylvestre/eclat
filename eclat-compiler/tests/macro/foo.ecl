let rec f x = x ;;

let main () =
  let n = 10 in
  for i = 0 to n-1 do print_int i done ;;
  (* print_int (let y = f 1 and z = f 2 and u = f 4 in y + z + u) *)