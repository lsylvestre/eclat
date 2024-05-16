#### Simulation for 7 queens

(stack size=3000 heap size = 6000 per semi-space)

```bash
$ make vm SRC=benchs/queens/queens.ml
$ make simul NS=400000000 

  pc:0|acc:1<int>|sp:1000|env:1<int>
  pc:22|acc:1<int>|sp:1000|env:1<int>
  GC-ALLOC:(size=2)
  pc:26|acc:4000<ptr>|sp:1001|env:1<int>

  ... // takes around 3 minutes in RTL simulation
         around 483_000 instructions are executed.
  ...
  ======> 40 // display the result (queens(7)) 
  ...
  STOP : cycle:3116939     // 3116939 cycles = 62 milliseconds at 50 MHz
  STOP : cycle:3116942     
  ...
^C
$
```

*=> 12 garbage collections*

-----

#### Simulation for 7 queens with an external function


```bash
$ make vm SRC=benchs/queens/queens_with_ext.ml CUSTOM=benchs/queens/queens_ext.ecl
$ make simul NS=400000000

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:22|acc:1<int>|sp:1000|env:1<int> 
  GC-ALLOC:(size=2) 

  ... // takes around 2 minutes in RTL simulation
         around 250_000 instructions are executed.
  ...
  ======> 40 // display the result (queens(7)) 
  ...
  STOP : cycle:1897671  // 1897671 cycles = 38 milliseconds at 50 MHz
  STOP : cycle:1897674    
...
^C
$
```

*=> 12 garbage collections*



**=> The version using an external function is 62/38=1.6 faster than the pure bytecode version**

