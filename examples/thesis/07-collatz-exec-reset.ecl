(* 

$ ./eclat ../examples/thesis/06-collatz-exec.ecl \
          ../examples/thesis/07-collatz-exec-reset.ecl \
          -arg="(1,false);(3,false);(8,false);(-2,false);(-2,false);(-2,false);(-2,false);(-2,false);(5,true);(7,false);(1,true);(8,false)"
    vhdl code generated in ../target/main.vhdl 
    testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.
$ make simul
    cd ../target; make NS=2000
    ghdl -a  runtime.vhdl stdlib.vhdl 
    ghdl -a  main.vhdl
    ghdl -a  tb_main.vhdl
    ghdl -e  tb_main
    ghdl -r  tb_main --vcd=tb.vcd --stop-time=2000ns
    ghdl:info: simulation stopped by --stop-time @2us
$ gtkwave ../target/tb.vcd

     ~> see 07-collatz-exec-reset.png

*)

let main ((n,r) : int<16> * bool) =
  let (o,rdy) = exec collatz(n) default 0 reset r 
  in o ;;