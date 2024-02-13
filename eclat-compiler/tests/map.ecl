let static t = 10^100;;

let map2_t f =
  let rec loop (i) =
    if i >= t.length-1 then () else
    let i_plus_1 = i+1 in
    let () = t[i] <- f (t[i])
    and () = t[i_plus_1] <- f (t[i_plus_1]) in
    loop(i+2)
  in
  loop(0) ;;


let counter () = 
  reg (fun c -> c + 1) last 0 ;;

let main () =
  
  let c = counter () in

  exec 
    let () = map2_t (fun _ -> counter ()) in
    print_string "cy:"; print_int c; print_newline ();
    map2_t (fun x -> print_string "==> "; print_int x; print_newline (); x);
    print_string "cy:"; print_int c; print_newline ();
    ()
  default ();;