#### Length
```bash
$ make vm SRC=benchs/list_length/length.ml
$ make simul NS=400000000 

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:50|acc:1<int>|sp:1000|env:1<int> 
  GC-ALLOC:(size=2) 
  pc:53|acc:4000<ptr>|sp:1000|env:1<int> 
  
  ... // takes around 10 sec. in RTL simulation
         around 4500 instructions are executed.
  ...
  ======> 200 // display the result
  ...
  STOP : cycle:238087   // 238087 cycles = 4.76 milliseconds at 50 MHz
  STOP : cycle:238090     
  ...
^C
$
```

*=> 0 garbage collection*

-----

#### Length with an external function


```bash
$ make vm SRC=benchs/list_length/length_with_ext.ml CUSTOM=benchs/list_length/length_ext.ecl
$ make simul NS=400000000

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:50|acc:1<int>|sp:1000|env:1<int> 
  GC-ALLOC:(size=2) 
  pc:53|acc:4000<ptr>|sp:1000|env:1<int> 

  ... // takes around 1 sec. in RTL simulation
         around 2600 instructions are executed.
  ...
  ======> 1 // display the result
  ...
  STOP : cycle:85615   // 50072 cycles = 1.7 milliseconds at 50 MHz
  STOP : cycle:85618     
  ...
^C
$
```

*=> 0 garbage collection*



**=> The version using an external function is 4.76/1.7=2.8 times faster than the pure bytecode version**

**=> if we consider only the computation time of `length` (without the list creation), the speedup is 17.** To measure this, we print all clock cycles (cycle:1, cycle:2 ...) by editing 
the main function in `ocaml-vm/main.ecl`

