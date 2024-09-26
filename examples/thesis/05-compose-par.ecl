(* 

$ ./eclat ../examples/thesis/01-fib.ecl \
          ../examples/thesis/05-compose-par.ecl \
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

     ~> see 05-compose-par.png

*)


let main () =
  let (o,rdy) = exec (let n = 4 + 1 in
                      let x = fibonacci(n) in
                      let (y, z) = ( fibonacci(3) || fibonacci(x) ) 
                      in y + z)
               default 0
  in o ;;
