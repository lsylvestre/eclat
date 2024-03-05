let static tab = 100^100;;

let rec pause () = () ;;

let rec loop(id,n) = (* loop for ever *)
  let _ = tab[id] <- tab[id] in print_int id;pause ();
  let _ = tab[id] <- tab[id] in print_int id;pause ();
  let _ = tab[id] <- tab[id] in print_int id;pause ();
  loop(id,n-1) ;;

let main () =
  let x1 = loop(1,100) 
  and x2 = loop(2,100)
  and x3 = loop(3,100) 
  and x4 = loop(4,100) 
  and x5 = loop(5,100) in
  () ;;

