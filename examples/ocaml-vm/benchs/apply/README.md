#### Simulation for `apply`

(stack size=3000, heap size = 6000 per semi-space)

```bash
$ make vm SRC=benchs/apply/apply.ml
$ make simul NS=400000000 

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:30|acc:1<int>|sp:1000|env:1<int> 
  GC-ALLOC:(size=2) 
  pc:33|acc:4000<ptr>|sp:1000|env:1<int> 

  ... // takes around 6 minutes in RTL simulation
         around 655_000 instructions are executed.
  ...
  ======> 65537 // display the result
  ...
  STOP : cycle:5113517     // 5113517 cycles = 102 milliseconds at 50 MHz
  STOP : cycle:5113520
$
```

*=> 42 garbage collections*
