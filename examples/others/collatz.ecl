(* 



   $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/others/collatz.ecl -main=collatz -arg="6" -relax

  (option -arg is used for simulation only and ignored for synthesis)

   $ cd ..
   $ make simul

*)



let collatz(n) =
  let rec loop(n,t) =
    if n = 1 then t else
    if n mod 2 = 0 then loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(n,0) ;;

let main(n) =
  let x = collatz n in 
  collatz(x) ;;
