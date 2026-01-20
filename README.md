## Eclat

A functional-imperative (ML-like) language with synchronous semantics
- for programming FPGAs 
- and designing reactive embedded systems mixing computations and interaction.


Installation
------------

To build from source, the pre-requisites are:

* `opam` 
* `ocaml 4.14.1`
* `menhir`
* `ocamlclean`
* for simulation: `ghdl` and `GTKWave`
* for synthesis: tested essentialy with `Intel Quartus II 24 lite` 
  on a Terasic DE10-lite board (having an Intel MAX 10 FPGA)
  Vivado and Yosys backend are also supported.

