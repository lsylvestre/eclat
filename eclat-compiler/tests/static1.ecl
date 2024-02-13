
let static c = 0^10 ;;

let main () =
    let x = reg (fun x -> x + 1) last 0 in
    exec
      c[0] <- x;         (* write takes one cycle *)
      print_int (c[0]);  (* read takes two cycles *)
      print_string ","
    default () ;;


(* to run: $ ./eclat tests/static1.ecl ; make simul

   ~> 1,5,9,13,17,21,25,29,33,37,41,45,49,5... *)