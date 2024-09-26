(* 

$ ./eclat ../examples/thesis/01-fib.ecl \
          ../examples/thesis/03-counter.ecl \
          ../examples/thesis/04-counter-fibonacci.ecl \
            -relax -arg="0;3;1;2;6;4;1;10;10;10;-3"
    vhdl code generated in ../target/main.vhdl 
    testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.
$ make simul NAME=counter
    cd ../target; make NS=2000
    ghdl -a  runtime.vhdl stdlib.vhdl 
    ghdl -a  main.vhdl
    ghdl -a  tb_main.vhdl
    ghdl -e  tb_main
    ghdl -r  tb_main --vcd=tb.vcd --stop-time=2000ns
    ghdl:info: simulation stopped by --stop-time @2us
$ gtkwave ../target/tb.vcd

     ~> see 04-counter-fibonacci.png

*)


let main n =
  let (o,rdy) = exec fibonacci(n) default (42 + n) in 
  let c = counter(rdy) in
  (o,c) ;;