
let main () = () ;;

let main () = main (); print_int 42 ;;

   (* =================
   $ ./eclat tests/good/main/t3.ecl
   $ make simul
     424242424242424...
   ==================== *)