(* ./eclat -relax *)
let f k =
  k(create<5>()) ;;

let main () =
  f (fun a -> f (fun b ->
      (get(a,0) || get(b,0)))) ;;

(* 
    because array locations are statically associated to [create<>] call sites,
    arrays created with a same call to [create<>] are conservately assumed to 
    be a same array in the generated code. 
    This lead to an acceptable source of imprecision in the very specific case
    where a function [f] creates an array and passes it to a continuation function [k]
    calling [f].

    Here, main has type [unit -[2]-> unit] but has a measured 
    execution time of exactly [1] cycle.

*)