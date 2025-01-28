(* 

$ ./eclat ../benchs/examples/14-environment.ecl \
          -arg="()"
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

     ~> see 14-environment.png

*)


let sum i =
  reg (fun s -> s + i) init 0 ;;

let environment o =
  if o < 10 then o + 1 
  else (-1) ;;

let main () =
  reg (fun pre_o ->
    let i = environment pre_o in
    let o = sum i in
    o) 
  init 0 ;;