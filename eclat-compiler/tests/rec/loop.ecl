let main () = 
  let rec f (x:bool) =
    let rec g (y:int) = if x then g 1 else f true in
    g 0
  in f true;;

(* to run: $ ./eclat tests/rec/loop.ecl -relax

   ~> 62,242,2042,2042,2042,... *)