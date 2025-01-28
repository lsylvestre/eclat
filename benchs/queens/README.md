## A reative application computing the number of solution to the N-queen problem.

### Measure execution time

```
$ ./eclat ../benchs/queens/nqueens.ecl \
               -arg="(false,false,0);(false,false,0);(false,false,0);\
                     (true,false,0);(true,true,0);(false,true,0);(false,false,0)"
vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.

$ make simul NS=10000000
cd ../target; make NS=10000000
ghdl -a  runtime.vhdl stdlib.vhdl 
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=10000000ns
[find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find]rang 1: 4 
[find][find][find][find][find][find][find]rang 2: 8 
rang 3: 16 
[find]rang 4: 18 
[find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find]rang 5: 18 
rang 8: 4 
[find]rang 6: 16 
rang 7: 8 
cy:<resize>4819

|number of solutions:92

[find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find][find]
^C
```

