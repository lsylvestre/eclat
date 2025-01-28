(* 

$ ./eclat ../benchs/examples/10-fby.ecl \
          -main="fby" \
          -arg="(42,0);(42,1);(43,2);(25,2);(8,2);(8,-1);(8,-2);(8,-3)"
    vhdl code generated in ../target/fby.vhdl 
    testbench generated in ../target/tb_fby.vhdl for software RTL simulation using GHDL.
$ make simul NAME=fby
    cd ../target; make NS=2000
    ghdl -a  runtime.vhdl stdlib.vhdl 
    ghdl -a  fby.vhdl
    ghdl -a  tb_fby.vhdl
    ghdl -e  tb_fby
    ghdl -r  tb_fby --vcd=tb.vcd --stop-time=2000ns
    ghdl:info: simulation stopped by --stop-time @2us
$ gtkwave ../target/tb.vcd

     ~> see 10-fby.png

*)

let fby ((x,y) : 'B * 'B) : 'B =
  let (o,_) = reg (fun (_,pre_y) -> (pre_y,y)) 
              init (x,x)
  in o ;;