
let main () =
  if true then false 
  else Int.get_bit((0:int<4>),1024);;

  (* because this [if/then/else] is combinational,
     this tempting to implement it as a mux in the generated code,
     but this is not correct because operators with size (such as get_bit) 
     could fail if computed speculatively *)