let static tab = 10^10;;


let main () =
  
  let c = reg (fun c -> c + 1) last 0 in
  print_string "cy:"; print_int c; print_newline ();

  exec
    (let rec loop i = 
      if i >= tab.length then () 
      else (tab[i] <- i; loop(i+1))
    in loop 0);
    print_string "<<<============================"; print_newline ();
    let a = (print_int(tab[0]); print_string ";";
             print_int(tab[2]); print_string ";"; 
             print_newline ())
    and b = (print_int(tab[1]); 
             print_newline ()) in
    print_string "============================>>>"; print_newline ();
    () 
  default ();;