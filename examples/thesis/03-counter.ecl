(* 

$ ./eclat ../examples/thesis/03-counter.ecl \
            -arg="true;true;false;false;true;true;false;true;true;true;false" \
            -main=counter
    vhdl code generated in ../target/counter.vhdl 
    testbench generated in ../target/tb_counter.vhdl for software RTL simulation using GHDL.
$ make simul NAME=counter
    cd ../target; make NS=2000
    ghdl -a  runtime.vhdl stdlib.vhdl 
    ghdl -a  counter.vhdl
    ghdl -a  tb_counter.vhdl
    ghdl -e  tb_counter
    ghdl -r  tb_counter --vcd=tb.vcd --stop-time=2000ns
    ghdl:info: simulation stopped by --stop-time @2us
$ gtkwave ../target/tb.vcd

     ~> see 03-counter.png

*)


let counter i =
  let o = reg (fun c -> if i then c + 1 else c) init 0
  in o ;;