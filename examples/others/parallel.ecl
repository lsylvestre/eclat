 (* A Parallel Éclat function:
   ==========================

       val gcd : (int * int) -> int 
       val main : int ⇒ (int * bool)


   $ cd eclat-compiler
   $ make
   $ ./eclat ../examples/counter.ecl \
             ../examples/gcd.ecl \
             ../examples/parallel.ecl \
                   -arg="1;1;1;2;2;2;2;6;3;6;7;8"

    (option -arg is used for simulation only and ignored for synthesis)
*)

let static ram = (5:int<42>)^512 ;;

let display (msg,n) =
   print_string(msg); print_int(n); print_newline ();;


let main(y) = (* y can change at each clock tick *)
  
  let cy = counter(false) in
  
  display ("cy=", cy); (* print the current clock cycle *)

  exec (let rec loop ((i,acc) : (int<10> * int)) =
          display ("i=", i); (* print: for simulation only *)
          if i >= ram.length then acc
          else (let v1 = ram[i] in
               let v2 = ram[i+1] in
               (* read v1 and v2 sequentially *)
               let x1 = gcd(v1,y) and x2 = gcd(v2,y) in
               display("    x1=",x1);
               display("    x2=",x2);
               (* compute x1 and x2 in parallel *)
               loop(i+2,acc + x1 + x2))
        in loop(0,0)) default 0

;;