(* 

  A sequential circuit in Éclat
  =============================

        val counter : bool => int


   $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/sum.ecl -arg="2;1;-3;2;-1;-2;3"

  (option -arg is used for simulation only and ignored for synthesis)

   $ cd ..
   $ make simul
    
      ~> Then, double-click on the file target/tb.vcd to open it with GtkWave 
         and add I/Os of the generated hardware to see their evolution

         A screen copy of the tool GTKWave, on this example, 
         is given in examples/sum.png

*)

let sum (i:int<8>) =
  let update(s) = s + i in 
  reg update last 0 ;;

let main(i) =
  let x = sum(i) in
  let y = sum(x) in
  let z = if i > 0 then 42 
          else sum(-10)
  in (x,y,z) ;;