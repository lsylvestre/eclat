let main () =
  let r = ref 5 in
let fact n = 
   let rec aux n = aux n + !r
in aux 5 in
fact (5:int<5>) + fact (5:int<5>);;