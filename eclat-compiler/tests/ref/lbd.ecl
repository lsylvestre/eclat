
let main () =
  let g () =
    let y = ref 5 in
    let y2 = y in
    let t0 = 10 in
    let u (x,t) = 
      let z = x in
      let rec h () = !z + !y  + t + t0
      in h () 
    in
    (* let rec f x = x + !y in
    f 2*)
    
    let g = ref 10 in
    u(g,42) + 1
  in print_int (g () + g ()) ;;