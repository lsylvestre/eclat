
let static c = 0^10 ;;

let main () =
  let rec f x =
    let c : int<32> = reg (fun y -> y + 1) last 0 in
    let rec g x = f x in
    g c
  in
  reg (fun _ ->
    let rec h2 u = f (u * 2) in
    exec
      (
        let rec h u = f (u + 1) in
       let x = h2 (h 42) in
       c[0] <- x;
       print_int (c[0]))
    default ())
  last ((),true) ;;

