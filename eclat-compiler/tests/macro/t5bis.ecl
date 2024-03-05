let static tab = 100^100;;

let rec pause () = () ;;

let rec loop(id,n) = 
  if n < 1 then () else
  let _ = tab[id] <- tab[id]+1 in print_int id;pause ();
  let _ = tab[id] <- tab[id]+1 in print_int id;pause ();
  let _ = tab[id] <- tab[id]+1 in print_int id;pause ();
  loop(id,n-1) ;;

let counter () =
  reg (fun c -> c + 1) last 0 ;;


let main () =
  let c = counter () in
  let ((),rdy) = 
    exec 
  let x1 = loop(1,100) 
  and x2 = loop(2,100)
  and x3 = loop(3,100) 
  and x4 = loop(4,100) 
  and x5 = loop(5,100) in
  () 
 default ()
  in if rdy then (print_newline() ; print_string "cy:"; print_int c; print_newline ()) ;;



