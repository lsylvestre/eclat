#### Simulation for BST

(stack size=3000 heap size = 6000 per semi-space)

```bash
$ make vm SRC=benchs/bst/bst.ml
$ make simul NS=400000000 

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:55|acc:1<int>|sp:1000|env:1<int> 
  GC-ALLOC:(size=2)
  pc:59|acc:4000<ptr>|sp:1001|env:1<int> 
  GC-ALLOC:(size=2)
  pc:63|acc:4002<ptr>|sp:1002|env:1<int> 
  pc:65|acc:10<int>|sp:1002|env:1<int> 
  pc:67|acc:200<int>|sp:1003|env:1<int> 
  pc:69|acc:5<int>|sp:1004|env:1<int> 
  ======> 5

  ... // takes around 1 minute in RTL simulation
         around 97_000 instructions are executed.
  ...
  ======> 42
  ...
  STOP : cycle:643241     // 643244 cycles = 12.9 milliseconds at 50 MHz
  STOP : cycle:643244     
  ...
^C
$
```

*=> 2 garbage collection*

-----

#### Simulation for BST with an external function


```bash
$ make vm SRC=benchs/bst/bst_with_ext.ml CUSTOM=benchs/bst/bst_ext.ecl
$ make simul NS=400000000 

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:55|acc:1<int>|sp:1000|env:1<int> 
  GC-ALLOC:(size=2) 
  pc:59|acc:4000<ptr>|sp:1001|env:1<int> 
  GC-ALLOC:(size=2) 
  pc:63|acc:4002<ptr>|sp:1002|env:1<int> 
  pc:65|acc:10<int>|sp:1002|env:1<int> 
  pc:67|acc:200<int>|sp:1003|env:1<int> 
  pc:69|acc:5<int>|sp:1004|env:1<int> 
  ======> 5

  ... // takes around 0.5 minute in RTL simulation
         around 41_000 instructions are executed.
  ...
  ======> 42
  ...
  STOP : cycle:322518     // 322518 cycles = 6.4 milliseconds at 50 MHz
  STOP : cycle:322521     
...
^C
$
```

*=> 2 garbage collections*



**=> The version using an external function is 12.9/6.4=2 faster than the pure bytecode version**

