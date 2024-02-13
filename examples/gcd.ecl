(* 

  A tail-recursive function in Éclat:
  ==================================
  
    val gcd : (int * int) -> int 


   $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/gcd.ecl \
             -main=gcd \
             -arg="(18,12);(15,15)" \
             -relax

    (option -arg is used for simulation only and ignored for synthesis)
    (option -relax allows compiling a non reactive program, e.g., gcd)

   $ cd ..
   $ make simul
    
      ~> Then, double-click on the file target/tb.vcd to open it with GtkWave 
         and add I/Os of the generated hardware to see their evolution

         A screen copy of the tool GTKWave, on this example, 
         is given in examples/gcd.png
*)

let rec gcd(a,b) =
  if a < b then gcd(a,b-a)
  else if a > b then gcd(a-b,b) 
  else a ;;
