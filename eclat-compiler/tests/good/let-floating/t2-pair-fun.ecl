(* ./eclat tests/good/let-floating/t2-pair-fun.ecl *)
let main () =
  let ((f,g),x) = 
    let z = ( 
      (* the moved up binding (z) must be processed too *)
      let ((f,g),x) = 
      let z = 55 + 1 in
      ( ( (fun x -> x), 
           let u = print_newline (); 56 * 2 in 
           (fun y -> y) ), 
        1 ) in
    let y = g(x) in
    56 + 1
    )
    in (* z must be moved up to allow 
                         the pair of function to be 
                         destructured *)
    ( ( (fun x -> x), 
         let u = print_newline (); 56*2 in 
         (fun y -> y) ), 
      1 ) in
  let y = g(x) in
  print_int (f(y)) ;;
