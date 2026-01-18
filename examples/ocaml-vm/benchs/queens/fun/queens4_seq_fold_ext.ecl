(* make SRC="benchs/queens/fun/queens4.ml"  FLAGS+="-ocaml" BCOPT="-load-code -load-data -custom caml_queens" CUSTOM=ocaml-vm/benchs/queens/fun/queens4_seq_fold_ext.ecl  byte

 *)
(* valeur absolue d'une soustraction *)
let abs_sub (a, b) = if b > a then b-a else a-b;;

(* two queens p and q are safe if : 
      - not in the same column
      - their vertical distance is not equal to their horizontal distance 
*)

let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;

let sz = 10 ;;

let safe_all (n,p,chess) =
  macro_generate (fun (i,acc) -> 
    acc & ((i+1 > n) or safe(vect_nth(chess,resize_int<32>(i)), p, n-i))
  ) true sz ;; 

let alloc_block(tag,size,st) =
  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
  let (acc,env,blk) = pause_make_block(sp,acc,env,tag,size) in
  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  (blk,st) ;;

let vect2array(vec,st) =
  let n = vect_size(vec) in
  let (blk,st) = alloc_block(0,resize_int<16>(n),st) in
  for j = 0 to n - 1 do
    let vv = val_long(as_long(resize_int<16>(vect_nth(vec,j)))) in
    set_field(blk,resize_int<16>(j),vv)
  done;
  (blk,st) ;;

let caml_cons ((v,l),st) =
  let (l2,st) = alloc_block(1,2,st) in
  set_field(l2,0,v);
  set_field(l2,1,l);
  (l2,st) ;;

let empty_list =
  val_long(0) ;;

let rec loop (id,restart,col, i, n, chess, acc, st) =
 if i <= n then (
   if safe_all (col,i,chess) then 
     ( let chess = vect_copy_with(chess,col,i) in
       let (acc2,st3) = if col+1 = n then 
                          let (blk,st2) = vect2array(chess,st) in
                          caml_cons((blk,acc),st2)
                        else (acc,st) in
       loop(id,restart,col+1, 1, n, chess, acc2, st3) 
     )
   else 
     loop(id,restart,col, i+1,n,chess, acc, st) )
 else 
   (if col >= restart then 
      loop(id,restart,col-1, vect_nth(chess,(resize_int<32>(col-1)))+1, n,chess, acc, st)
    else (acc,st))
;;


let queens_seq (n,v, st) (* :int<8> * int<8> vect<'a> * _ *) =
  loop (0,1, 1, 1,n,v,empty_list, st) ;;

let caml_queens ((v,_),st) = print_string "hello!!!";
  let chess = vect_create<20> (1:int<8>) in
  let (acc,st) = queens_seq(resize_int<8>(long_val v),chess,st) in
  (acc, st)  ;;


