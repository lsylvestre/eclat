(*

 $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/gcd.ecl \
             ../examples/example.ecl \
             -main=example \
             -arg="()" \
             -relax

    (option -arg is used for simulation only and ignored for synthesis)
    (option -relax allows compiling a non reactive program, e.g., gcd)

   $ cd ..
   $ make simul
    
      ~> A screen copy of the tool GTKWave, on this example, 
         is given in examples/example.png

*)

type int8 = int<8> ;;

let example() =
  let x = gcd(2,2) in
  let y = x + 1 in
  let z = gcd(5,10) in
  let x1 = gcd(18,12) and 
      x2 = gcd(5,10) in
  let s = x1 + x2 in 
  ( (x,y,z,s) : int8 * int8 * int8 * int8) ;;
