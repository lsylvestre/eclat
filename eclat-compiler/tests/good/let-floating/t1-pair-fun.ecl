(* ./eclat tests/good/let-floating/t1-pair-fun.ecl *)
let main () =
  let ((f,g),x) = 
    let z = 55 + 1 in (* z must be moved up to allow 
                         the pair of function to be 
                         destructured *)
    ( ( (fun x -> x), 
         let u = print_newline (); 56*2 in 
         (fun y -> y) ), 
      1 ) in
  let y = g(x) in
  print_int (f(y)) ;;
