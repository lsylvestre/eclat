# WIP : new compilation scheme for Eclat

### Experimental compilation scheme for Eclat by flattening Eclat programming constructs into an Eclat subset, directly translated to VHDL, without using an imperative VHDL-like intermediate language (Mini-HDL)

see:
- eclat-compiler/src/Eclat/compile/backend/other-compiler/compilation.ml
- eclat-compiler/src/Eclat/compile/backend/other-compiler/gen_vhdl2.ml
- eclat-compiler/src/Eclat/compile/backend/other-compiler/gen_testbench2.ml
- eclat-compiler/src/Eclat/compile/middle_end/normalize/anf2.ml
- eclat-compiler/src/Eclat/compile/compile.ml