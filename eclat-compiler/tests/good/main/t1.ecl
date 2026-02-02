
let main () = () ;;

let foo x = x + 1 ;;

   (* =================
   $ ./eclat tests/good/main/t1.ecl -main=foo -arg="5;4;6" -ocaml
   ocaml code generated in ../target/ml/foo_step.ml
   vhdl code generated in ../target/foo.vhdl 
   testbench generated in ../target/tb_foo.vhdl for software RTL simulation using GHDL.
   ==================== *)