## A worker-farm based parallel implementation of map

### Measure execution time

```
$ ./eclat ../benchs/skeletons/map_farm/map_farm.ecl

vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.

$ make simul NS=10000000
cd ../target; make NS=10000000
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=10000000ns
 cy=8085 
 cy=16170 
 cy=24255 
 cy=32340 
^C
```

