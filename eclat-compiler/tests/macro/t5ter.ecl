let static tab = 100^100;;
let static tab2 = 55^100;;

let foo () =
    tab.(0) <- 100;
    let () = tab.(0) <- 42
    (* and x = tab.(1) *)
    and y = tab.(0) in
    print_int(y) ;;


let main () = foo();;