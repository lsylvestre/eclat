## Synthesis on an OrangeCrab board (Lattice ECP5 FPGA) using an open source tool chain


Installation
------------

To build from source, the pre-requisites are:

* `oss-cad-suite` (https://github.com/YosysHQ/oss-cad-suite-build)
* `ghdl-yosys-plugin` (https://github.com/ghdl/ghdl-yosys-plugin)

From this directory:

```
$ make init
$ make front
$ make synth
$ make pnr
$ make pack
$ make dfu

$ make clean
```

*Note: currently, the implementation of the OCaml VM in Eclat 
does not meet the timing constraints of this FPGA target (FAIL at place&route: `make pnr`)*