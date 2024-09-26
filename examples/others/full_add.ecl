(* 

   A combinational circuit in Éclat:
   ================================

       val full_add : (bool * bool * bool) => (bool * bool) 
       val half_add : (bool * bool) => (bool * bool)


   $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/full_add.ecl \
                   -main=full_add \
                   -arg="(false,false,false);(false,false,true);(false,true,false);(false,true,true);(true,false,false);(true,false,true);(true,true,false);(true,true,true)"


    (option -arg is used for simulation only and ignored for synthesis)
*)

let half_add(a,b) = 
  let s = a xor b in
  let co = a & b in
  (s,co)
;;

let full_add(a,b,ci) =
  let (s1,c1) = half_add(a,b) in
  let (s,c2) = half_add(ci,s1) in
  let co = c1 or c2 in
  (s, co)
;;