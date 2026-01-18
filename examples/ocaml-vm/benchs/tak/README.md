#### Simulation for `tak(18, 12, 6)`

(stack size=3000, heap size = 6000 per semi-space)

```bash
$ make vm SRC=benchs/tak/tak.ml
$ make simul NS=400000000 

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:48|acc:1<int>|sp:1000|env:1<int> 
  GC-ALLOC:(size=2) 
  pc:52|acc:4000<ptr>|sp:1001|env:1<int> 

  ... // takes around 8 minutes in RTL simulation
         around 1_113_000 instructions are executed.
  ...
  ======> 7 // display the result
  ...
  STOP : cycle:7501269     // 7501269 cycles = 150 milliseconds at 50 MHz
  STOP : cycle:7501270
$
```

*=> 42 garbage collections*
