
let comparator ((go,(op,a,b)) : (bool * (compare_op * long * long))) : bool * bool =
  (****************************************************
   no need to suspend the execution when input [go] is set 
   because the body of the comparator is combinational
   ****************************************************)
  let x1 = (a = b) in
  let x2 = (a < b) in
  let b = match op with
          | EQ() -> x1
          | LT() -> x2
          | NEQ() -> not(x1)
          | GE() -> not(x2)
          | LE() -> x1 or x2
          | GT() -> not(x1 or x2)
  (****************************************************
   output [rdy] is always true because the comparator is instantaneous 
   ****************************************************)
  let rdy = true in
  (b,rdy) ;;
