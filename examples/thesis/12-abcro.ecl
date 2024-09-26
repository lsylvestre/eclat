(* 

$ ./eclat ../examples/thesis/10-fby.ecl \
          ../examples/thesis/11-abro.ecl \
          ../examples/thesis/12-abcro.ecl \
          -arg="(true,false,false,false);(false,true,true,false);(true,true,true,false);(false,true,false,true);(true,true,true,false)"
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

     ~> see 12-abcro.png

*)

let abcro (a,b,c,r) = 
  let t = abro(a,b,r) in
  let o = abro(t,c,r)
  in o ;;

let flat_abcro (a,b,c,r) = 
  let o = edge ((await(a,r) & 
                 await(b,r)) & 
                 await(c,r))
  in o ;;

let main (a,b,c,r) =
  let o = abcro(a,b,c,r) in
  assert (not (o xor flat_abcro(a,b,c,r)));
  o ;;
