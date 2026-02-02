
type t = A of int | B of unit ;;

let main (x:t) =
  match x with
  | A(n) -> A(n+1)
  | B() -> B() ;;


   (* =================
   $ ./eclat tests/good/main/t2.ecl -main=foo -arg="A(5);A(4);B()" -ocaml
   ocaml code generated in ../target/ml/foo_step.ml
   vhdl code generated in ../target/foo.vhdl 
   testbench generated in ../target/tb_foo.vhdl for software RTL simulation using GHDL.
   ==================== *)