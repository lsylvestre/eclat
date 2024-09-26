(* 

$ ./eclat ../examples/thesis/06-collatz-exec.ecl \
          -arg="1;3;8;-2;-2;-2;-2;-2;5;7;1;8"
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

     ~> see 06-collatz-exec.png

*)

let collatz n =
  let rec loop(i,t) =
    if i = 1 then t 
    else if i mod 2 = 0 then loop(i / 2, t + 1)
         else loop(3 * i + 1, t + 1)
  in 
  loop(n,1) ;;

let main n =
  let (o,rdy) = exec collatz n default 0 
  in o ;;