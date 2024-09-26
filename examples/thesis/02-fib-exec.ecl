(* 

$ ./eclat ../examples/thesis/01-fib.ecl \
          ../examples/thesis/02-fib-exec.ecl \
            -arg="0;3;1;2;6;4;1;10;10;10;-3;1"
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

     ~> see 02-fib-exec.png

*)

let main n =
  if n < 0 then 0 else
  let (o,rdy) = exec fibonacci(n) default 42
  in o ;;
