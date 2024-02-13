#### Gcd
```bash
$ make vm SRC=benchs/queens/queens.ml
$ make simul NS=400000000 

pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:2|acc:50000<int>|sp:1000|env:1<int> 
  pc:4|acc:42<int>|sp:1001|env:1<int> 

  ======> 42
  
  ... // takes around 4 minutes in RTL simulation
         around 550000 instructions are executed.
  ...
  ======> 1 // display the result of  gcd(1,50_000)
  ...
  STOP : cycle:3450139   // 3450139 cycles = 69 milliseconds at 50 MHz
  STOP : cycle:3450142     
  ...
^C
$
```

*=> 0 garbage collection*

-----

#### Gcd with an external function


```bash
$ make vm SRC=benchs/gcd/gcd_with_ext.ml CUSTOM=benchs/gcd/gcd_ext.ecl
$ make simul NS=400000000

  pc:0|acc:1<int>|sp:1000|env:1<int> 
  pc:2|acc:50000<int>|sp:1000|env:1<int> 
  pc:4|acc:42<int>|sp:1001|env:1<int> 

  ======> 42

  ... // takes around 4 minutes in RTL simulation
         8 instructions are executed.
  ...
  ======> 1 // display the result of  gcd(1,50_000)
  ...
  STOP : cycle:50072   // 50072 cycles = 1.0 milliseconds at 50 MHz
  STOP : cycle:50075     
  ...
^C
$
```

*=> 0 garbage collection*



**=> The version using an external function is 69 times faster than the pure bytecode version**

