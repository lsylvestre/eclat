
## Eclat

A functional-imperative (ML-like) language with synchronous semantics

- for programming FPGAs 
- and designing reactive embedded systems mixing computations and interaction.


```
$ cd eclat-compiler
$ make
$ ./eclat -arg "6;5;4;3;7;5;8;3;9;5"
=== eclat toploop ===.
Enter phrases (separated by ';;') then compile (or run) with ``#q.''

> let sum i = 
    reg (fun s -> i + s) init 0;;
val sum : (int<~z137> -{0}-> int<~z137>)

> let main (i:int<20>) : int<20> * int<20> =
    let o1 = sum(i) in    
    let o2 = sum(i) in 
    assert (o1 = o2);
    print_int o1; print_newline ();
    (o1, o2);;
val main : (int<20> -{0}-> (int<20> * int<20>))

> #q.
$ make simul
cd ../target; make NS=2000
ghdl -a  runtime.vhdl stdlib.vhdl 
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=2000ns
6 
11 
15 
18 
25 
30 
38 
41 
50 
55
...
```

Installation
------------

To build from source, the pre-requisites are:

* `opam` 
* `ocaml 4.14.1`
* `menhir`
* `ocamlclean`
* for simulation: `ghdl` and `GTKWave`
* for synthesis: `Intel Quartus II 22.1 lite`, on a 
  Terasic DE10-lite board (having an Intel MAX 10 FPGA)


Application example : implementing the OCaml virtual machine and a runtime for OCaml
------------

```
$ cd eclat-compiler
$ make
$ cd ../ocaml-vm
$ make SRC=tests/fibo.ml
$ make simul
(cd ../target; make NS=50000   )
ghdl -a  runtime.vhdl stdlib.vhdl 
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=50000ns
pc:0|acc:1<int>|sp:1000|env:1<int> 
pc:22|acc:1<int>|sp:1000|env:1<int> 
GC-ALLOC:(size=2) 
size:1 
pc:26|acc:4000<ptr>|sp:1001|env:1<int> 
pc:28|acc:11<int>|sp:1001|env:1<int> 
pc:29|acc:4000<ptr>|sp:1002|env:1<int> 
ENV:1<int> 
pc:2|acc:4000<ptr>|sp:1005|env:4000<ptr> 
pc:3|acc:11<int>|sp:1005|env:4000<ptr> 
pc:9|acc:11<int>|sp:1005|env:4000<ptr> 
pc:10|acc:11<int>|sp:1005|env:4000<ptr> 
pc:12|acc:9<int>|sp:1005|env:4000<ptr> 
pc:13|acc:4000<ptr>|sp:1006|env:4000<ptr> 
ENV:4000<ptr> 
pc:2|acc:4000<ptr>|sp:1009|env:4000<ptr> 
pc:3|acc:9<int>|sp:1009|env:4000<ptr> 
pc:9|acc:9<int>|sp:1009|env:4000<ptr> 
pc:10|acc:9<int>|sp:1009|env:4000<ptr> 
pc:12|acc:7<int>|sp:1009|env:4000<ptr> 
pc:13|acc:4000<ptr>|sp:1010|env:4000<ptr> 
ENV:4000<ptr> 
...
```
