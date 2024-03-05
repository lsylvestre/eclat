let static m1 = false^20^20;;
let static m2 = false^20^20;;

let counter () =
  reg (fun c -> c + 1) last 0 ;;

let main () =
  let c = counter () in
  
  let (_,rdy) =  exec
    for i = 1 to m1[0].length-1 do
      for j = 1 to m2[1].length-1 do
        m2.(i).(j) <- m2.(i).(j)
      done
    done
    default ()
  in 
  if rdy then (print_newline() ; print_string "cy:"; 
    print_int c; print_newline ()) ;;
