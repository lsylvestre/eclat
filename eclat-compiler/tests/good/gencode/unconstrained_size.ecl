let main ((x:int<2*(1+1+1+?n)>):int<?s>) = 
  (42:int<?s+1>);;

(* 

./eclat tests/good/gencode/unconstrained_size.ecl -Wsize -ocaml
Warning: Unknown value size in the generated code replaced by 32-bit default size.
ocaml code generated in ../target/ml/main_step.ml
vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.

*)