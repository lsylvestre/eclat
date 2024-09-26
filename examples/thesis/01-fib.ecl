(* 

$ ./eclat ../examples/thesis/01-fib.ecl -main=fibonacci -relax -arg="0;3;1;2;6;4;1;10;11;12;-3"
   vhdl code generated in ../target/fibonacci.vhdl 
   testbench generated in ../target/tb_fibonacci.vhdl for software RTL simulation using GHDL.
$ make simul NAME=fibonacci
   cd ../target; make NS=2000
   ghdl -a  runtime.vhdl stdlib.vhdl 
   ghdl -a  fibonacci.vhdl
   ghdl -a  tb_fibonacci.vhdl
   ghdl -e  tb_fibonacci
   ghdl -r  tb_fibonacci --vcd=tb.vcd --stop-time=2000ns
   ghdl:info: simulation stopped by --stop-time @2us
$ gtkwave ../target/tb.vcd 

    ~> see 02-fib.png

*)

let fibonacci n =
  let rec fib (i, a, b) =
    if i = 0 then a
    else fib(i - 1, b, a + b) 
  in
  fib(n, 0, 1) ;;
