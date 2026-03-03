(* ./eclat -relax *)

let main () =
  let x = create<45>() in
  let y = create<30>() in
  let z = create<45>() in
  let a = create<24>() in
  let b = create<43>() in
  [ set(x,0,42) || set(y,0,43) || set(z,0,43) || 
    [ set(a,1,43) || set(b,2,43) ]
  ];;

(* val main : (unit -[1]-> unit) *)