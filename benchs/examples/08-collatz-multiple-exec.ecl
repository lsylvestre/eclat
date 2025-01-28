(* 

$ ./eclat ../benchs/examples/06-collatz-exec.ecl \
          ../benchs/examples/08-collatz-multiple-exec.ecl \
          -arg="(2,8);(2,8);(2,8);(2,8);(2,8);(1,1);(1,1);(1,1);(1,1);(1,1)"
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

     ~> see 08-collatz-multiple-exec.png

*)

let main (a,b) : (int<8> * int<8> * (int<8> * int<8>)) =
  let (x,rdy1) = exec collatz(a) default 0 in
  let (y,rdy2) = exec collatz(b) default 0 in
  let (o,rdy1) = exec (collatz(a) || collatz(b)) default (0,0) in
  (x,y,o) ;;