Eclat 
=====

`Eclat` can be seen both as a hardware description 
with high-level features or an high-level 
(functional-imperative, OCaml-like) language with RTL semantics.

Tail-recursion
===

Executing a tail-recursive function performs a pause of one clock cycle at each (direct or recursive) call, such as:

```
let rec gcd (a,b) =
  if a < b then gcd (a,b-a) else
  if a > b then gcd (a-b,b)
  else a ;;
```

Combinational circuits
===

Functions that do not contain tail-recursion nor array accesses are instantaneous:
```
let half_add(a,b) = 
  let s = a xor b in
  let co = a & b in
  (s,co) ;;
``````

Here the function `half_add` is a classical combinational circuit taking two boolean input values and returning their sum with a carry output.
The function is called at each clock tick and is typically connected to physical I/Os or other hardware components.

Sequential circuits
===

The Eclat construct `reg f init v` is a register initialized with `v`
and updated with function `f`. Note that this is a Mealy machine: the first execution of `reg f init v` returns `f(v)`, the next returns `f(f(v))`, etc.

For instance, here is a counter:
```
let count inc = 
  reg (fun inc -> c + inc) init 0;;
```

Types
=====

Eclat is statically typed. The type language is as follows:

```
type       -- ty ::= tyB | ty * ty | ty -dur-> tyB | tyB array<sz> | 'a
basic type -- tyB ::= bool | unit | tyB x<sz> | tyB * tyB 
                    | (X_1 of tyB_1 | ... X_n of ty_N)
                    | `a
duration   -- dur := n | max(dur,dur) | `d
size       -- sz := n | sz + n | 2 * sz | `s
```

Immediate values, such as booleans, are typed with basic types.
They are implemented as bitvectors in the RTL code generated.

Other values are specialized (removed) by the compiler.
For example, higher-order function are inlined. 
Note that the return type of a function must be a basic type, 
as enforced by the type constructor `ty -dur-> tyB`. 
As a consequence, all Eclat functions have one argument (e.g., a pair).

Type variables are noted:
```
'a, `a, `d, `s
```

Syntax of Eclat programs
=======

Eclat programs are sequences of declarations:
- ```type x = ty ;;``` is an alias `x` for type `ty` ;
- ```type x = X_1 of tyB_1 | ... X_n of ty_N``` define a sum type ;
- ```type tyB x<sz> ;;``` defines an abstract basic type parametrized by a basic type `tyB` and a size `sz`, e.g., 'a vect<sz> ;
   * ```type tyB x ;;``` is an abreviation for `tyB x<1>`
- ```operator M.f : tyB1 -> tyB2``` define the external operator `M.f`
- ```external f : tyB1 -> tyB2``` define the external function `f`
- ```let x = e ;;``` define the value `x`




Example
=====

$ cd eclat-compiler
$ make
$ ./eclat -arg "6;5;4;3;7;5;8;3;9;5"
=== eclat toploop ===.
Enter phrases (separated by ';;') then compile (or run) with ``#q.''

> let sum i = 
    reg (fun s -> i + s) init 0;;
val sum : forall '289  . (int<~z289> -{0}-> int<~z289>) | 0

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
...
```