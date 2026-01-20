operator%with_sizes Vect2.create : `a => `a vect<'size> ;;
let vect2_make <<'s>> (x:`a) : `a vect<'s> = Vect2.create(x) ;;


let main () =
  let a = vect2_make<<64>>(false) in
  let w = vect2_make<<65>>(5) in
  (a,w);;



(**** problem: 
<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 ./eclat -relax tests/todo/poly.ecl -nostdlib -arg="()"
file ../v13.ecl, line 7, characters 10-20:
Error: 
This expression has type ((size<65> * int<~z9>) -{d34}-> `b33) but was expected of type ((size<65> * bool) -{0}-> (bool) vect<65>)

  Hint: An expression has basic type int<~z9> but was expected of basic type bool
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

strangely, this work in the toplevel:

<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$ ./eclat                            
=== eclat toploop ===.
Enter phrases (separated by ';;') then compile (or run) with ``#q.''

> 

operator%with_sizes Vect2.create : `a => `a vect<'size> ;;
let vect2_make <<'s>> (x:`a) : `a vect<'s> = Vect2.create(x) ;;


let main () =
let a = vect2_make<<64>>(false) in
let w = vect2_make<<65>>(5) in
  (a,w);;
> val vect2_make : forall '1080  . ((size<~z1080> * `b1079) -{0}-> (`b1079) vect<~z1080>) | 0

> 
val main : forall '1083  . (unit -{0}-> ((bool) vect<64> * (int<~z1083>) vect<65>)) | 0

> #q.

vhdl code generated in ../target/main.vhdl 
testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
**********)

