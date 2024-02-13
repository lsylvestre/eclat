#### Simulation for `share`

(stack size=3000, heap size = 6000 per semi-space)

```bash
$ make vm SRC=benchs/apply/apply.ml
$ make simul NS=400000000 

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:37|acc:1<int>|sp:1000|env:1<int> 
  GC-ALLOC:(size=2) 
  pc:40|acc:4000<ptr>|sp:1000|env:1<int> 

  ... // takes around 0.5 minute in RTL simulation
         around 7000 instructions are executed.
  ...
  ======> 42
  ...
  STOP : cycle:451083     // 451083 cycles = 9.0 millisecond at 50 MHz
  STOP : cycle:451084
$
```

*=> 1 garbage collection*
