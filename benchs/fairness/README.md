## Fig. 8: Fair interlacing of concurrent memory accesses in an Eclat program


```
$ ./eclat ../benchs/fairness/fairness.ecl  -relax

vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.

$ make simul
cd ../target; make NS=2000
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=2000ns
0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;
```

