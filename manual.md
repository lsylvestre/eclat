
# Eclat 

The `Eclat` language is a hardware description language
with high-level (functional-imperative, OCaml-like) features. 
It can also be seen as high-level language with RTL semantics
and hardware implementation.

`Eclat` is compiled to VHDL for simulation and synthesis on FPGA boards.

## Overview

Eclat programs describes circuits processing input streams to produce output streams. A program is a function that read current inputs and computes current outputs. For example, the following program has two input signal a and b and returns their sum as output:
```
let main (a,b) = 
  a + b;;
```

The body of the program is an expression. The evaluation of an expression computes a value by spaning one or more clock ticks of a synchronizing clock.
A clock cycle is the duration between two clock ticks.

Any expression evaluable during a single clock tick, is said *instantaneous* : its duration is zero cycle (neglecting the propagation time of electric signal in the hardware implementation).

The duration of an expression depends on the environment (i.e., variables that occurs free in the expression) and the semantics of each programming construct, with a simple rule:

```
all Eclat constructs are instantaneous, except pauses, recursive function calls and array accesses^(1), which delay the execution until the next clock tick.

(1) Concurrent array accesses are sequentialized, resulting in additional cycles, in a deterministic and predictable way.
```

A function is said instantaneous if, for any argument, it computes a result instantaneously. 

Below are main Eclat features:

### Tail-recursion


Tail-recursion allows for iterative execution with 
a pause of one clock cycle at each (direct or recursive) call.
For instance, here is an Eclat definition of the GCD 
(Greatest Common Diviser) algorithm:

```
let rec gcd (a,b) =
  if a < b then gcd (a,b-a) else
  if a > b then gcd (a-b,b)
  else a ;;
```
### Parallelism

The `(e1||... en)` construct allows for parallel execution of expressions `e1` ... `en` in lock-step.

For example, the following function computes the greatest common diviser
of four arguments:

```
let gcd3 (a,b,c,d) =
  let (x,y) = (gcd(a,b) || gcd(c,d)) in
  gcd(x,y) ;;
```

### Arrays

Arrays constitute a regular, mutable data structure, 
efficiently implemented with RAM blocks on FPGA targets.
Array reads and writes take 1 cycle. Each array 
is protected by a single lock for both reads and writes.
Array creation and accessing the length of an array 
are instantaneous operations. Type checking avoid dynamic allocation.
```
let main() =
  let a = create<2048>() in
  for i = 0 to length(a) - 1 do
    set(a,i,42)
  end
```

### Sequential circuits


The Eclat construct `reg f init e0` is a register initialized with expression `e0`
and updated with function `f`. Note that this is a Mealy machine: the first execution of `reg f init e0` returns `f(e0)`, the next execution returns `f(f(e0))`, etc.

For instance, here is a counter definition starting by 1.
```
let count () = 
  reg (fun 0 -> c + 1) init 0;;
```

### Mixing interaction and computation

The Eclat construct `exec e defaut e0` aims to repeatedly computes the expression `e`, which is typically a multi-cycle expression, within an instantaneous expression. Each time `exec e defaut e0` is executed, a single step is performed in the computation of `e`. If the obtained redex is a value `v`, then the pair `(v,true)` is returned by `exec`. Otherwise, if the redex is a reductible expression `e1`, then `exec` returns the pair `(e0,false)` where `e0` is an instantaneous expression computing a default value.

For instance, the following program executes the GCD algorithms while incrementing a counter at each cycle:  

```
let main (a,b) =
  let (x,rdy) = exec gcd(a,b) default 0 in
  let c = count () in 
  (x,rdy,c) ;;
```

## Expression language


The syntax of the Eclat expression language 
is shown below:

```
e ::=                         -- expression
// ML subset
    | x
    | c
    | (e1, ... en)
    | e1 e2
    | fun p -> e
    | let p = e1 in e2
    | if e1 then e2 else e3
    | let rec f = v in e
    | (e1 || ... en)
    | (e : ty)

// Eclat-specific constructs
    | reg (fun p -> e) init e0
    | exec e1 defaut e0 reset e2   // [reset e2] is optional

// assertions
    | assert e

// imperative features
    | e1 ; e2
    | for x = e1 to e2 do e3 done
    | create<sz>()    // create an array
    | get(x,e) 
    | set(x,e,e)
    | length(x)

// macro-expansion
    | parfor x = sz to sz do e done
    | iterate<sz to sz> (fun p -> e) init e0

// sum types
    | Ctor e1
    | match e with Ctor1 p1 -> e1 `|` ... Ctorn pn -> en [ `|` _ -> e] // the last case is optional

// vectors
    | vect_create<sz>(e)
    | {e1, ... en}

// size
    | << sz >>

// Esterel-like features
    | pause
    | trap x in e
    | exit x
    | signal<>
    | emit x(e)
    | ?x
    | suspend e when x
    | loop: e end
    | [e1 || e2]

// Lustre-like features
    | e where rec p1 = e1 and ... pn = en
    | e fby e
    | e when e
    | merge(x,e1,e2)

p ::= () | x | (p1, ... pn)  -- pattern

c ::= () | true | false      -- constant
    | (c1, ... cn)           * tuple
    | {c1, ... cn}           * vector
    | op                     * operator

```

## Types and primitives

Eclat is statically and implicitely typed using a variant of the ML type system. The type language is as follows:

```
type        -- ty ::= tyB 
                    | ty_1 * ... ty_n 
                    | ty -{dur}-> tyB 
                    | tyB array<sz> 
                    | << sz >>
                    | 'a
basic type  -- tyB ::= bool 
                    | unit
                    | tyB x<sz>
                    | tyB_1 * ... tyB_n 
                    | (X_1 of tyB_1 | ... X_n of ty_N)
                    | string
                    | ~a
duration    -- dur ::= 
* instantaneous       0
* multi-cycle       | 1
                    | max(dur,dur)
                    | $a
size        -- sz ::= n | sz + n | 2 * sz | ?a
type scheme -- sigma ::= ty | forall v1 ... vN . sigma
            -- vi    ::= 'a | ~a | $a | ?a
```

Immediate values, such as booleans, are typed with basic types.
They are implemented as bitvectors in the RTL code generated.

Other values are specialized (removed) by the compiler.
For example, higher-order function are inlined. 
Note that the return type of a function must be a basic type, 
as enforced by the type constructor `ty -{dur}-> tyB`. 
As a consequence, all Eclat functions have one argument (e.g., a pair).

Type variables are noted: ```'a, ~a, `$a, ?a```.

We use some syntactic sugar: 
* `ty => tyB` for `ty -{0}-> tyB`
* `ty -> tyB` for `ty -{1}-> tyB`

The main Eclat primitives have the following type signatures:

```
(=) : forall `a . (`a * `a) => bool
(or) : (bool * bool) => bool
(&) : (bool * bool) => bool
(xor) : (bool * bool) => bool
not : bool => bool

abs : forall `sz . int<`sz> => int<`sz>
(+) : forall `sz . (int<`sz> * int<`sz>) => int<`sz>
(-) : forall `sz . (int<`sz> * int<`sz>) => int<`sz>
( * ) : forall `sz . (int<`sz> * int<`sz>) => int<`sz>
(/) : forall `sz . (int<`sz> * int<`sz>) => int<`sz>
(mod) : forall `sz . (int<`sz> * int<`sz>) => int<`sz>
get_bit : forall `sz . (int<`sz> * int<32>) => bool
update_bit : forall `sz . (int<`sz> * int<32> * bool) => int<`sz>
int_resize : forall `sz1 `sz2 . (<<`sz2>> * int<`sz1>) => int<`sz2>

vect_nth: forall `sz `a . (`a vect<`sz> * int<32>) => `a
vect_size: forall `sz `a . `a vect<`sz> => int<32>
vect_copy_with: forall `sz `a . (`a vect<`sz> * int<32> * `a) => `a vect<`sz> 
vect_make : forall `sz `a . (<<`sz>> * `a) => `a vect<`sz>

print_string : string => unit
print_int : forall `sz . int<`sz> => unit
print_newline : unit => unit
```

Primitives such as `print_string`, `print_int` and `print_newline` are used for simulation only, but are not synthesizable. 
The compiler flag `-no-print` can be used to remove call to these functions
in the generated RTL code.

## Structure of Eclat programs

Eclat programs are sequences of declarations:
- ```type (~a1, ... ~aN) x<?n1,...?nM> = tyB ;;``` introduces an alias `x` for basic type `tyB`; type variables in `tyB` must belongs to: `~a1, ... ~aN, ?n1,...?nM`) ;
- ```type (~a1, ... ~aN) x<?n1,...?nM> = X_1 of tyB_1 | ... X_n of ty_N``` defines a sum type ; type variables in `tyB` must belongs to: `~a1, ... ~aN, ?n1,...?nM`) ;
- ```type (~a1, ... ~aN) x<?n1,...?nM> ;;``` defines an abstract basic type parametrized by basic type `(~a1, ... ~aN)` and sizes `?n1,...?nM`, e.g., `~a vect<?n>`;
- ```operator M.f : tyB1 => tyB2``` defines the external operator `M.f`
- ```external f : tyB1 -{dur}-> tyB2``` defines the external function `f`
- ```shared external f : tyB1 -{dur}-> tyB2``` defines shared external function `f`
- ```let x = e ;;``` defines the value `x`


## Compile a program

to compile a file `foo.ecl`, enter in the `eclat-compiler` directory
and execute the command:
```
$ ./eclat foo.ecl -arg="i1;i2;i3"
```

where `i1;i2;i3` is a sequence of input values (used for testbench generation).

This generates two files, `main.vhdl` and `tb_main.vhdl` in the `target` folder.
To simulate the VHDL design with GHDL:
```
$ cd ../target
$ make NS=50000
```

where `NS` is the duration of the simulation (in nano-seconds)
with a frequency of 100 MHz (i.e., a clock period of 10 ns.) 

If the Eclat program is not instantaneous, 
the type system rejects it with the following message :

```
Error: This program has type (t1 -> t2). It is not reactive. 
```

To disable this check, use the compier flag `-relax` ;
e.g., 

```
./eclat gcd.ecl -relax -arg="(5,15)"
```

If a program `main1` is not instantaneous, its behaviour is equivalent to
that of the instantanous program:
```
let main(i) =
  exec main1(i) default c0
```
with `c0` a constant of same type than `main1(i)`.


If several `.ecl` files are passed to the compiler `./eclat`, these files are concatenated with left-to-right order.

By default, the compiler includes the file `eclat-compiler/stdlib.ecl`, which is the Eclat standard library. Operators defined in this file are implemented in the runtime library `target/stdlib.vhdl`.

The compiler flag `-nostdlib` prevents the load of `eclat-compiler/stdlib.ecl`.

If no `.ecl` file is passed to the compiler, the compiler enters in a toplevel read-type-print loop. For instance:

```
$ ./eclat -arg "6;5;4;3;7;5;8;3;9;5"
=== eclat toploop ===.
Enter phrases (separated by ';;') then compile (or run) with ``#q.''

> let sum i = 
    reg (fun s -> i + s) init 0;;
val sum : forall ?n870  . (int<?n870> => int<?n870>) | 0

> let main (i:int<20>) : int<20> * int<20> =
    let o1 = sum(i) in    
    let o2 = sum(i) in 
    assert (o1 = o2);
    print_int o1; print_newline ();
    (o1, o2);;
val main : (int<20> => (int<20> * int<20>)) | 0

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