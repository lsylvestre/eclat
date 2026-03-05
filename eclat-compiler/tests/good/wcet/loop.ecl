let f s =                     
  for i = 0 to int(s) - 1 do print_int i done;;

let w () = f <<10>> ;;

let main () =
  exec w () default () ;;