
let compose (f,g,x) = f (g(x)) ;;

let main () =
  let f x = x + 1 in
  let g y = y + 2 in
  let v = compose (f,g,4) in
  let f1 x = if x then 1 else 2 in
  let g1 y = y or false in
  let w = compose (f1,g1,true) in w;;

(*
let id x = x ;;

let main () =
  if id true then (id 4) else 5;;*)