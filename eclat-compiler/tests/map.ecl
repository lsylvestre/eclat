(* $ ./eclat tests/map.ecl
   $ make simul NS=4000 
 *)

let static tab = 100^128;;
let static tab2 = 100^128;;

let mapi_1(f)<src,dst> =
  let rec loop (i) =
    if i >= src.length - 1 then () else
    let update(i) = dst[i] <- f (i,src[i]) in
    let () = update(i) in
    loop(i+1)
  in
  loop(0) ;;


let mapi_2(f)<src,dst> =
  let rec loop (i) =
    if i >= src.length - 1 then () else
    let update(i) = dst[i] <- f (i,src[i]) in
    let () = update(i)
    and () = update(i+1) in
    loop(i+2)
  in
  loop(0) ;;

let mapi_4(f)<src,dst> =
  let rec loop (i) =
    if i >= src.length - 1 then () else
    let update(i) = dst[i] <- f (i,src[i]) in
    let () = update(i)
    and () = update(i+1)
    and () = update(i+2)
    and () = update(i+3) in
    loop(i+4)
  in
  loop(0) ;;

let mapi_8(f)<src,dst> =
  let rec loop (i) =
    if i >= src.length - 1 then () else
    let update(i) = dst[i] <- f (i,src[i]) in
    let () = update(i)
    and () = update(i+1)
    and () = update(i+2)
    and () = update(i+3)
    and () = update(i+4)
    and () = update(i+5)
    and () = update(i+6)
    and () = update(i+7) in
    loop(i+8)
  in
  loop(0) ;;


let sum(n) = 
  let rec loop(acc,n) =
    if n < 1 then acc else loop(acc+n,n-1)
  in loop(0,n) ;;

let counter () = 
  reg (fun c -> c + 1) last 0 ;;

let main () =
  let c = counter () in
  exec 
    print_string "cy:"; print_int c; print_newline ();
    let () = (mapi_8 (fun (i,_) -> sum(i)))<tab,tab2> in
    print_string "cy:"; print_int c; print_newline ();
    (mapi_8 (fun (i,x) -> print_string "==> "; print_int x; print_newline (); x))<tab2,tab2>;
    ()
  default ();;
