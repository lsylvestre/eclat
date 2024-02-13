(* 

   $ cd eclat-compiler
   $ make
   $ ./eclat tmp/platform.ecl -intel-max10

*)

let vhex1 = (true,false,true,false,true,false,true,false) ;;

let vhex2 = (false,true,false,true,false,true,false,true) ;;

let main (((sw0,sw1,sw2,sw3,sw4,sw5,sw6,sw7,sw8,sw9),(key1,key2)) : 
          ((bool * bool * bool * bool * bool * bool * bool * bool * bool * bool) *
           (bool * bool))) =
  
  let led0 = sw0 in
  let led1 = sw1 in
  let led2 = sw2 in
  let led3 = sw3 in
  let led4 = sw4 in
  let led5 = sw5 in
  let led6 = sw6 in
  let led7 = sw7 in
  let led8 = sw8 in
  let led9 = sw9 in

  let hex0 = if key1 then vhex1 else vhex2 in
  let hex1 = if key1 then vhex2 else vhex2 in
  let hex2 = if key1 then vhex1 else vhex2 in
  let hex3 = if key2 then vhex1 else vhex2 in
  let hex4 = if key2 then vhex1 else vhex2 in
  let hex5 = if key2 then vhex2 else vhex1 in

  let leds = (led0,led1,led2,led3,led4,led5,led6,led7,led8,led9) in
  let hexs = (hex0,hex1,hex2,hex3,hex4,hex5) in
  (leds,hexs)

;;