(* 

$ ./eclat ../examples/thesis/10-fby.ecl \
          ../examples/thesis/11-abro.ecl \
          -main="abro" \
          -arg="(true,false,false);(false,true,false);(true,true,false);(false,false,true);(true,true,false)"
    vhdl code generated in ../target/abro.vhdl 
    testbench generated in ../target/tb_abro.vhdl for software RTL simulation using GHDL.
$ make simul NAME=abro
    cd ../target; make NS=2000
    ghdl -a  runtime.vhdl stdlib.vhdl 
    ghdl -a  abro.vhdl
    ghdl -a  tb_abro.vhdl
    ghdl -e  tb_abro
    ghdl -r  tb_abro --vcd=tb.vcd --stop-time=2000ns
    ghdl:info: simulation stopped by --stop-time @2us
$ gtkwave ../target/tb.vcd

     ~> see 11-abro.png

*)

let await (i,r) =
  let o = reg (fun s -> 
               (s or i) & not r) 
          init false
  in o ;;

let edge i = 
  (not (fby(false,i))) & i ;;

let abro (a,b,r) = 
  let o = edge (await (a,r) & 
                await(b,r))
  in o ;;
