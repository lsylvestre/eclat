
(* $ make SRC="../benchs/queens/fun/config.ml \
               ../benchs/queens/fun/solve.ml \
               ../benchs/queens/fun/queens.ml" \
               MORE_OPT="-I=../benchs/queens/fun" \
               FLAGS="-ocaml" \ BCOPT="-load-code -load-data" \
               byte
   $ make vm
   $ cd ../target/ml
   $ ocamlc runtime.ml main_step.ml
   $ ./a.out -run 400000000


   (*  CY=20207779 NBI=4997105 *)
 *)

(* ********************************** *)

open Config
open Solve

let queens n = 
  let stk = Array.create n 1 in
  queens_with (n, stk, []) ;;


(* let solutions = for i = 0 to 999 do queens(8) done ;;*)
let solutions = queens(10) in
print_int 42 ;; (* (List.length solutions) *) ;;
