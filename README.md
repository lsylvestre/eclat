# HLPP24 special issue: Programming parallelism on FPGAs with Eclat

Eclat (*declarative language*) is an OCaml-like programming language, which is compiled to hardware descriptions for reconfiguring FPGAs.

The language has a synchronous semantics to control timing, throughput and parallelism in the applications.

This artefact is an extension of the Eclat compiler with shared memory (mutable arrays) implemented as RAM blocks on a FPGA. This extension supports concurrent memory accesses in a fair and predictable way.


Installation
------------

To build from source, the pre-requisites are:

* `opam` 
* `ocaml 4.14.1`
* `menhir`
* for simulation: `ghdl`
* for synthesis: `Intel Quartus II 22.1 lite`, on a 
  Terasic DE10-lite board (having an Intel MAX 10 FPGA)

Simulation
---------
```
$ cd eclat-compiler
$ make
$ ./eclat -relax ../benchs/game-of-life/v1/v1.ecl  -main=test_main

vhdl code generated in ../target/test_main.vhdl 
testbench generated in ../target/tb_test_main.vhdl for software RTL simulation using GHDL.

$ make simul NS=40000 NAME=test_main
cd ../target; make NS=40000
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=40000ns
 
*-*----- 
-**----- 
-*------ 
-------- 
-------- 
-------- 
-------- 
--------
...
```

Synthesis
---------

```
$ ./eclat -relax ../benchs/game-of-life/v1/v1.ecl  -intel-max10  -main=main_intel

vhdl code generated in ../target/main_intel.vhdl 
testbench generated in ../target/tb_main_intel.vhdl for software RTL simulation using GHDL.
```
