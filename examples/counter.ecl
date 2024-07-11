(* 

  A sequential circuit in Éclat
  =============================

        val counter : bool => int


   $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/counter.ecl \
                   -main=counter \
                   -arg="false;false;false;true;true;false;false"

  (option -arg is used for simulation only and ignored for synthesis)

   $ cd ..
   $ make simul
    
      ~> Then, double-click on the file target/tb.vcd to open it with GtkWave 
         and add I/Os of the generated hardware to see their evolution

         A screen copy of the tool GTKWave, on this example, 
         is given in examples/counter.png

*)

let counter(rst) =
  let inc(c) = 
    if rst then 0 else c + 1 
  in
  reg inc init 0 ;;