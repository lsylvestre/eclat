(* 

$ ./eclat ../examples/thesis/09-map-vect.ecl \
          -arg="({1,2,3,4,5,6,7,8,9,10,11,12} : int<8> vect<12>)"
    vhdl code generated in ../target/main.vhdl 
    testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.
$ make simul
    cd ../target; make NS=2000
    ghdl -a  runtime.vhdl stdlib.vhdl 
    ghdl -a  main.vhdl
    ghdl -a  tb_main.vhdl
    ghdl -e  tb_main
    ghdl -r  tb_main --vcd=tb.vcd --stop-time=2000ns
    ghdl:info: simulation stopped by --stop-time @2us
$ gtkwave ../target/tb.vcd

     ~> see 09-map-vect.png

*)

let map (f,a) =           (* ((f,a) : (('B1 -> 'B2) * 'B1 vect<'N>)) : 'B2 vect<'N> = *)
  let rec loop (i,acc) =
    if i < vect_size a then
      let x = vect_nth (a,i) in
      let y = f x in
      loop(i+1,vect_copy_with(acc,i,y))
    else acc
  in loop(0,a) ;;

let main (a:int<8> vect<12>) : int<8> vect<12> =
  let zero : int<8> vect<12> = vect_create<12>(1) in
  let inc x = x + 1 in
  let (o,rdy) = exec map(inc,a) default zero
  in o ;;