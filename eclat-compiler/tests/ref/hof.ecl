
let rec foo x = 5 ;;

let main () =
  let f = foo (fun x -> x) in () ;;