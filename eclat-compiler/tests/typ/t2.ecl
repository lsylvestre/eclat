let main f =
  let rec pause () = () in
  let () = reg pause last () in
  ();;
(* let main () =
  let x = 5 in
  let f (y,z) =
    if true then (y,1) else (z,2) in 2 ;;
(* let main x =
  let f y = y + 1 in
  (reg f last 0) * 2 ;;*)
  (* let u = !r in
  let i y = if y then x else x+x in
  let rec f x = f x in
  if true then () else ();;*)

(*
let id x = x ;;

let main () =
  if id true then (id 4) else 5;;*)


let await (i,reset) =
  let step s = (s or i) & not reset in
  reg step last false ;;
  
let fby (a,b) =
  let step (x,_) = (b,x) in
  let (_,x) = reg step last (a,b) in x ;;

let edge i = not (fby(false,i)) & i ;;

let abro ((a,b),r) = edge (await (a,r) & await(b,r)) ;;

let abcro (((a,b),c),r) = abro ((abro((a,b),r),c),r) ;;
*)