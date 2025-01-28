## Game of Life, v2

*The world is represented by an immutable vector of booleans.*

### Measure execution time

*Since the circuit is combinational, 
 it computes the entire world within one clock cycle.*

```
$ ./eclat -relax ../benchs/game-of-life/v2/v2.ecl  -main=chrono_main
vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.

$ make simul NS=400 NAME=chrono_main
cd ../target; make NS=400
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=400ns
execution time = 1 cycles   // <-- once finished, the program restarts
execution time = 2 cycles 
execution time = 3 cycles 
execution time = 4 cycles
...
```

### Display successive generations of the world

```
$ ./eclat -relax ../benchs/game-of-life/v2/v2.ecl  -main=test_main
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
$ ./eclat ../benchs/game-of-life/v2/v2.ecl  -intel-max10  -main=main_intel
$ make simul NS=4000000 NAME=main_intel
```

Note: compilation is slow (about 1 minute for a world of size 64*64)
and the generated code is huge. 
Option `-moore` implement the resulting circuit as a Moore machine (Mealy machine by default).
This saves logic elements (LEs) in the MAX 10 FPGA (because within each LE, the combinational 
part is connected to the input of a register in the same LE).


#### for Xilinx Zybo FPGA

```
$ ./eclat ../benchs/game-of-life/v2/v2.ecl  -xilinx-zybo  -main=main_xilinx
```

#### for Yosys / ECP5

```
$ ./eclat ../benchs/game-of-life/v2/v2.ecl  -yosys-ecp5  -main=main_yosys
```