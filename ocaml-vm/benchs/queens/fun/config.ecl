let stk_size : int<6> = 20 ;;
let stk_create n = 
  if n > stk_size then 
    fatal_error "undersize stack" 
  else vect_create<20>(1) ;;

let stk_update(stk,i,v) = 
  vect_copy_with(stk,resize_int<16>(i),v)
;;
let stk_nth(stk,i) = 
  vect_nth(stk,resize_int<16>(i));;

let stk2array(stk, st) =
  let n = resize_int<16>(stk_size) in
  let (blk,st) = alloc_block(0,n,st) in
  for j = 0 to n - 1 do
    let q = stk_nth(stk,j) in
    set_field(blk,j,val_long(as_long(q)))
  done;
  (blk,st) ;;

let add_solution(stk,(l,st)) = 
  let (v,st') = stk2array(stk,st) in
  caml_cons((v,(l,())), st') ;;

let empty_list = val_long(0) ;;
