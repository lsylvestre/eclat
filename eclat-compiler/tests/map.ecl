(* $ ./eclat tests/map.ecl
   $ make simul NS=4000 *)

let static tab = 10^100;;

let mapi2_inplace f = ^ t .
  let rec loop (i) =
    if i >= t.length-1 then () else
    let i_plus_1 = i+1 in
    let () = t[i] <- f (i,t[i])
    and () = t[i_plus_1] <- f (i+1,t[i_plus_1]) in
    loop(i+2)
  in
  loop(0) ;;

let fact(n) = 
  let rec loop(acc,n) =
    if n < 2 then acc else loop(acc*n,n-1)
  in loop(1,n) ;;

let counter () = 
  reg (fun c -> c + 1) last 0 ;;

let main () =
  
  let c = counter () in

  exec 
     print_string "cy:"; print_int c; print_newline ();
    let () = (mapi2_inplace (fun (i,x) -> counter () * 100 + i))<tab> in
    print_string "cy:"; print_int c; print_newline ();
    (mapi2_inplace (fun (i,x) -> print_string "==> "; print_int x; print_newline (); x))<tab>;
    ()
  default ();;