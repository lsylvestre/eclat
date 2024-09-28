## A slice-based parallel implementation of map

### Measure execution time

```
$ ./eclat ../benchs/skeletons/map_slices/map.ecl 

vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.
$ make simul NS=10000000
cd ../target; make NS=10000000
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=10000000ns
 cy=4290 
 cy=8581 
 cy=12872 
 cy=17163
^C
```

