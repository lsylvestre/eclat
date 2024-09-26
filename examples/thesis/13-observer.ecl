(* 

$ ./eclat ../examples/thesis/13-observer.ecl \
          -arg="true;false;false;true;true;false;true;true;false"
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

     ~> see 13-observer.png

*)

let count(event) =
  reg (fun c -> 
        if event then c + 1 
        else c) init 0 ;;
 
let observer ((f,i),p) =
  let o = f(i) in
  let ok = p(i,o) in
  (o,ok) ;;

let always_positive (_,o) = 
  reg (fun s -> s & 0 < o) 
  init true ;;

let main(event) =
  let (o,ok) = 
    observer((count,event),
              always_positive)
  in ok ;;