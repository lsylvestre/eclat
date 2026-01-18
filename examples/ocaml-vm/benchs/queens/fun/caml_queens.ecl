let caml_queens ((v,_),st) =
  let n = resize_int<6>(long_val v) in
  let stk = stk_create(n) in
  let acc0 = (empty_list, st) in
  queens_with(n,stk,acc0) ;;