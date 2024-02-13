(* 

   $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/abcro.ecl \
             -arg="(((false,false),false),false);(((true,true),true),false);(((true,true),true),false);(((true,true),true),true);(((false,true),false),false);(((true,false),false),false);(((false,false),false),false);(((false,false),true),false)"

    (option -arg is used for simulation only and ignored for synthesis)
   
   $ cd ..
   $ make simul
    
      ~> Then, double-click on the file target/tb.vcd to open it with GtkWave 
         and add I/Os of the generated hardware to see their evolution

         A screen copy of the tool GTKWave, on this example, 
         is given in examples/mix.png
         This trace is hard to read because inputs (vs. outputs) are 
         concatenated and encoded in hexadimal
*)

let await (i,reset) =
  let step s = (s or i) & not reset in
  reg step last false ;;
  
let fby (a,b) =
  let step (x,_) = (b,x) in
  let (_,x) = reg step last (a,b) in x
;;

let edge i = not (fby(false,i)) & i ;;

let abro ((a,b),r) = edge (await (a,r) & await(b,r)) ;;

let abcro (((a,b),c),r) = abro ((abro((a,b),r),c),r) ;;

let main x = abcro x ;;

