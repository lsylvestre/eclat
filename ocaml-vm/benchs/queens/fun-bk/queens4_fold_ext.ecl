let caml_cons ((v,l),st) =
  let (l2,st) = caml_make_block((1,2),st) in
  set_field(l2,0,v);
  set_field(l2,1,l);
  (l2,st) ;;

let empty_list =
  val_long(0) ;;



(* make SRC="benchs/queens/fun/queens4.ml"  FLAGS+="-ocaml" BCOPT="-load-code -load-data -custom caml_queens" CUSTOM=ocaml-vm/benchs/queens/fun/queens4_fold_ext.ecl  byte

 *)
(* valeur absolue d'une soustraction *)
let abs_sub (a, b) = if b > a then b-a else a-b;;

(* two queens p and q are safe if : 
      - not in the same column
      - their vertical distance is not equal to their horizontal distance 
*)

let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;


(* une nouvelle reine p est safe vis-à-vis d'un ensemble de n reines q
 ssi  \forall i \ in[1..n] : S(q_i, p, n+1-i)
*)

let sz = 10 ;;

let safe_all (n,p,chess) =
  macro_generate (fun (i,acc) -> 
    acc & ((i+1 > n) or safe(vect_nth(chess,resize_int<32>(i)), p, n-i))
  ) true sz ;; 


let vect2array(vec,st) =
  let n = vect_size(vec) in
  let (blk,st) = caml_make_block((0,resize_int<16>(n)),st) in
  for j = 0 to n - 1 do
    let vv = val_long(as_long(resize_int<16>(vect_nth(vec,j)))) in
    set_field(blk,resize_int<16>(j),vv)
  done;
  (blk,st) ;;

let write_en = create<10>() ;;
let write_data = create<10>() ;;


let rec loop (id,restart,col, i, n, chess) =
 if i <= n then (
   if safe_all (col,i,chess) then 
     ( let chess = vect_copy_with(chess,col,i) in
       (if col+1 = n then (
        (* ************************ *)
        (* post a solution to write *)
        set(write_en,id,true);
        set(write_data,id,chess);
        (* wait end of writing *)
        let rec wait() =
          if get(write_en,id) then wait() else ()
        in wait()) else () );
        (* ************************ *)
       loop(id,restart,col+1, 1, n, chess) )
   else 
     loop(id,restart,col, i+1,n,chess) )
 else 
   (if col >= restart then 
      loop(id,restart,col-1, vect_nth(chess,(resize_int<32>(col-1)))+1, n,chess)
    else ())
;;

let queens ((n,v,st) : int<8> * int<8> vect<'a> * `b) =
  
  let finish = create<1>() in
  set(finish,0,false);

  let () = parfor i = 1 to sz do
              if i <= n then 
              let v = vect_copy_with(v,0,i) in 
              loop (i-1, 2, 1, 1, n, v)
            done;
            set(finish,0,true)
  and (acc,st) = 
            let rec wait(i,acc,st) =
              if get(finish,0) then (acc,st) else
              (if i < length write_en - 1 then
                (if get(write_en,i) then (
                  let chess = get(write_data,i) in
                  let (blk,st2) = vect2array(chess,st) in
                  let (acc2,st3) = caml_cons((blk,acc),st2) in
                  set(write_en,i,false);
                  wait(i+1,acc2,st3)
                ) else wait(i+1,acc,st)) else
               wait(0,acc,st))
            in wait(0,empty_list,st)
  in
  (acc,st);;


let caml_queens ((v,_),st) = print_string "hello!!!";
  let chess = vect_create<20> (1:int<8>) in
  queens(resize_int<8>(long_val v),chess,st) ;;

