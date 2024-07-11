(* 

  Reactive program mixing interaction and computation:
  ===================================================
  
    val gcd : (int * int) -> int 
    val counter : bool => int
    val main : (int * int * bool) => (int * int)


   $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/counter.ecl \
             ../examples/gcd.ecl \
             ../examples/mix.ecl \
             -arg="(18,12,false);(-1,-1,false);(-1,-1,true);(-1,-1,false);(15,15,false);(-1,-1,false)"

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

let main (a,b,rst) =
  let (x,rdy) = exec 
                  gcd(a,b) 
                default 0 in
  let r = rdy or rst in
  let c = counter(r) in
  (x,c) ;;