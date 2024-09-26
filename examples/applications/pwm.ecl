(* 

For synthesis on the Intel Max10 FPGA :

$ ./eclat ../examples/applications/pwm.ecl -intel-max10

    vhdl code generated in ../target/main.vhdl 
    testbench generated in ../target/tb_main.vhdl for software RTL simulation using GHDL.
*)


let fby ((x,y) : 'B * 'B) : 'B =
  let (o,_) = reg (fun (_,pre_y) -> (pre_y,y)) 
              init (x,x)
  in o ;;


let triangle(period) = 
  reg (fun c -> 
        if c >= period then 0 else c + 1
      ) init 0 ;;

let modulation(duty_cycle,period) =
  let c = triangle(period) in
  let o = (c < duty_cycle) in
  o ;;

let edge i = not (fby(false,i)) & i ;;

let min(a,b) = if a < b then a else b ;;
let max(a,b) = if a > b then a else b ;;

let period = 1024 ;;
let step = period / 16 ;;

let control_duty_cycle(inc,dec) =
  let dc = reg (fun v -> 
                  if edge(inc) then min(v + step, period) else 
                  if edge(dec) then max(v - step, 0) else
                  v) init period
  in dc ;;

let pwm((inc,dec) : bool * bool) =
  let dc = control_duty_cycle(inc,dec) in
  modulation(dc,period) ;;

let main ((sw,(inc_button,dec_button)) : bool vect<10> * (bool * bool)) : bool vect<10> * int<48> =
  let v = vect_mapi((fun (i,b) -> pwm(b & inc_button, b & dec_button)), sw) in
  (v,0) ;;
