## Game of Life, v3

*The world is represented by an array of immutable vectors of booleans.*

### Measure execution time

*Since the circuit is combinational, 
 it computes the entire world within one clock cycle.*

```
$ ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -main=chrono_main
vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.

$ make simul NS=40000 NAME=chrono_main
cd ../target; make NS=40000
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=40000ns
execution time = 28 cycles 
execution time = 56 cycles 
execution time = 84 cycles 
execution time = 112 cycles 
execution time = 140 cycles 
execution time = 168 cycles 
execution time = 196 cycles 
...
```

### Display successive generations of the world

```
$ ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -main=test_main
$ make simul NS=4000000 NAME=test_main
cd ../target; make NS=4000000
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=4000000ns
 
*-*----- 
-**----- 
-*------ 
-------- 
-------- 
-------- 
-------- 
-------- 

... (see trace-v2.txt)
```

### Synthesis

#### for Intel Max10 FPGA

```
$ ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -intel-max10  -main=main_intel
```

#### for Xilinx Zybo FPGA

```
$ ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -xilinx-zybo  -main=main_xilinx
```

#### for Yosys / ECP5

```
$ ./eclat -relax ../benchs/game-of-life/v3/v3.ecl  -yosys-ecp5  -main=main_yosys
```
