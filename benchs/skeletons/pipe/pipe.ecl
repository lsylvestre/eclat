let pipe_worker(f,(rI,src),(rO,dst)) =
  let rec loop(i) =
    if i < length src then
    (let j = get(rI,0) 
     and x = get(src,i) in
     if i < j then
        let y = f x in
        set(dst,i,y);
        set(rO,0,i+1);
        loop(i+1)
      else (loop(i)))
    else ()
  in 
  loop(0)  ;;


let pipe1 ((f,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let tmp1 = create<'N>() in 
  let rdy1 = create<1>() in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0) in
  let () = pipe_worker(f,(rdy0,src),(rdy1,tmp1))
  in k(tmp1)
;;

let pipe2 ((f1,f2,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let (tmp1,rdy1) = (create<'N>(), create<1>()) in
  let (tmp2,rdy2) = (create<'N>(), create<1>()) in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0)
  and () = set(rdy2,0,0) in
  let () = pipe_worker(f1,(rdy0,src),(rdy1,tmp1))
  and () = pipe_worker(f2,(rdy1,tmp1),(rdy2,tmp2))
  in k(tmp2)
;;

let pipe3 ((f1,f2,f3,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let (tmp1,rdy1) = (create<'N>(), create<1>()) in
  let (tmp2,rdy2) = (create<'N>(), create<1>()) in
  let (tmp3,rdy3) = (create<'N>(), create<1>()) in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0)
  and () = set(rdy2,0,0)
  and () = set(rdy3,0,0) in
  let () = pipe_worker(f1,(rdy0,src),(rdy1,tmp1))
  and () = pipe_worker(f2,(rdy1,tmp1),(rdy2,tmp2))
  and () = pipe_worker(f3,(rdy2,tmp2),(rdy3,tmp3))
  in k(tmp3)
;;

let pipe4 ((f1,f2,f3,f4,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let (tmp1,rdy1) = (create<'N>(), create<1>()) in
  let (tmp2,rdy2) = (create<'N>(), create<1>()) in
  let (tmp3,rdy3) = (create<'N>(), create<1>()) in
  let (tmp4,rdy4) = (create<'N>(), create<1>()) in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0)
  and () = set(rdy2,0,0)
  and () = set(rdy3,0,0)
  and () = set(rdy4,0,0) in
  let () = pipe_worker(f1,(rdy0,src),(rdy1,tmp1))
  and () = pipe_worker(f2,(rdy1,tmp1),(rdy2,tmp2))
  and () = pipe_worker(f3,(rdy2,tmp2),(rdy3,tmp3))
  and () = pipe_worker(f4,(rdy3,tmp3),(rdy4,tmp4))
  in k(tmp4);;

let pipe5 ((f1,f2,f3,f4,f5,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let (tmp1,rdy1) = (create<'N>(), create<1>()) in
  let (tmp2,rdy2) = (create<'N>(), create<1>()) in
  let (tmp3,rdy3) = (create<'N>(), create<1>()) in
  let (tmp4,rdy4) = (create<'N>(), create<1>()) in
  let (tmp5,rdy5) = (create<'N>(), create<1>()) in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0)
  and () = set(rdy2,0,0)
  and () = set(rdy3,0,0)
  and () = set(rdy4,0,0)
  and () = set(rdy5,0,0) in
  let () = pipe_worker(f1,(rdy0,src),(rdy1,tmp1))
  and () = pipe_worker(f2,(rdy1,tmp1),(rdy2,tmp2))
  and () = pipe_worker(f3,(rdy2,tmp2),(rdy3,tmp3))
  and () = pipe_worker(f4,(rdy3,tmp3),(rdy4,tmp4))
  and () = pipe_worker(f5,(rdy4,tmp4),(rdy5,tmp5))
  in k(tmp5);;

let pipe6 ((f1,f2,f3,f4,f5,f6,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let (tmp1,rdy1) = (create<'N>(), create<1>()) in
  let (tmp2,rdy2) = (create<'N>(), create<1>()) in
  let (tmp3,rdy3) = (create<'N>(), create<1>()) in
  let (tmp4,rdy4) = (create<'N>(), create<1>()) in
  let (tmp5,rdy5) = (create<'N>(), create<1>()) in
  let (tmp6,rdy6) = (create<'N>(), create<1>()) in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0)
  and () = set(rdy2,0,0)
  and () = set(rdy3,0,0)
  and () = set(rdy4,0,0)
  and () = set(rdy5,0,0)
  and () = set(rdy6,0,0) in
  let () = pipe_worker(f1,(rdy0,src),(rdy1,tmp1))
  and () = pipe_worker(f2,(rdy1,tmp1),(rdy2,tmp2))
  and () = pipe_worker(f3,(rdy2,tmp2),(rdy3,tmp3))
  and () = pipe_worker(f4,(rdy3,tmp3),(rdy4,tmp4))
  and () = pipe_worker(f5,(rdy4,tmp4),(rdy5,tmp5))
  and () = pipe_worker(f6,(rdy5,tmp5),(rdy6,tmp6))
  in k(tmp6);;

let pipe7 ((f1,f2,f3,f4,f5,f6,f7,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let (tmp1,rdy1) = (create<'N>(), create<1>()) in
  let (tmp2,rdy2) = (create<'N>(), create<1>()) in
  let (tmp3,rdy3) = (create<'N>(), create<1>()) in
  let (tmp4,rdy4) = (create<'N>(), create<1>()) in
  let (tmp5,rdy5) = (create<'N>(), create<1>()) in
  let (tmp6,rdy6) = (create<'N>(), create<1>()) in
  let (tmp7,rdy7) = (create<'N>(), create<1>()) in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0)
  and () = set(rdy2,0,0)
  and () = set(rdy3,0,0)
  and () = set(rdy4,0,0)
  and () = set(rdy5,0,0)
  and () = set(rdy6,0,0) 
  and () = set(rdy7,0,0) in
  let () = pipe_worker(f1,(rdy0,src),(rdy1,tmp1))
  and () = pipe_worker(f2,(rdy1,tmp1),(rdy2,tmp2))
  and () = pipe_worker(f3,(rdy2,tmp2),(rdy3,tmp3))
  and () = pipe_worker(f4,(rdy3,tmp3),(rdy4,tmp4))
  and () = pipe_worker(f5,(rdy4,tmp4),(rdy5,tmp5))
  and () = pipe_worker(f6,(rdy5,tmp5),(rdy6,tmp6))
  and () = pipe_worker(f7,(rdy6,tmp6),(rdy7,tmp7))
  in k(tmp7);;

let pipe8 ((f1,f2,f3,f4,f5,f6,f7,f8,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let (tmp1,rdy1) = (create<'N>(), create<1>()) in
  let (tmp2,rdy2) = (create<'N>(), create<1>()) in
  let (tmp3,rdy3) = (create<'N>(), create<1>()) in
  let (tmp4,rdy4) = (create<'N>(), create<1>()) in
  let (tmp5,rdy5) = (create<'N>(), create<1>()) in
  let (tmp6,rdy6) = (create<'N>(), create<1>()) in
  let (tmp7,rdy7) = (create<'N>(), create<1>()) in
  let (tmp8,rdy8) = (create<'N>(), create<1>()) in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0)
  and () = set(rdy2,0,0)
  and () = set(rdy3,0,0) 
  and () = set(rdy4,0,0) 
  and () = set(rdy5,0,0) 
  and () = set(rdy6,0,0) 
  and () = set(rdy7,0,0) 
  and () = set(rdy8,0,0) in
  let () = pipe_worker(f1,(rdy0,src),(rdy1,tmp1))
  and () = pipe_worker(f2,(rdy1,tmp1),(rdy2,tmp2))
  and () = pipe_worker(f3,(rdy2,tmp2),(rdy3,tmp3))
  and () = pipe_worker(f4,(rdy3,tmp3),(rdy4,tmp4))
  and () = pipe_worker(f5,(rdy4,tmp4),(rdy5,tmp5))
  and () = pipe_worker(f6,(rdy5,tmp5),(rdy6,tmp6))
  and () = pipe_worker(f7,(rdy6,tmp6),(rdy7,tmp7))
  and () = pipe_worker(f8,(rdy7,tmp7),(rdy8,tmp8))
  in k(tmp8)
;;

let pipe9 ((f1,f2,f3,f4,f5,f6,f7,f8,f9,src),k) =

  let _ = (src : `A array<'N>) in (* type constraint *)

  let rdy0 = create<1>() in
  let (tmp1,rdy1) = (create<'N>(), create<1>()) in
  let (tmp2,rdy2) = (create<'N>(), create<1>()) in
  let (tmp3,rdy3) = (create<'N>(), create<1>()) in
  let (tmp4,rdy4) = (create<'N>(), create<1>()) in
  let (tmp5,rdy5) = (create<'N>(), create<1>()) in
  let (tmp6,rdy6) = (create<'N>(), create<1>()) in
  let (tmp7,rdy7) = (create<'N>(), create<1>()) in
  let (tmp8,rdy8) = (create<'N>(), create<1>()) in
  let (tmp9,rdy9) = (create<'N>(), create<1>()) in
  let () = set(rdy0,0,(length src))
  and () = set(rdy1,0,0)
  and () = set(rdy2,0,0)
  and () = set(rdy3,0,0) 
  and () = set(rdy4,0,0) 
  and () = set(rdy5,0,0) 
  and () = set(rdy6,0,0) 
  and () = set(rdy7,0,0) 
  and () = set(rdy8,0,0) 
  and () = set(rdy9,0,0) in
  let () = pipe_worker(f1,(rdy0,src),(rdy1,tmp1))
  and () = pipe_worker(f2,(rdy1,tmp1),(rdy2,tmp2))
  and () = pipe_worker(f3,(rdy2,tmp2),(rdy3,tmp3))
  and () = pipe_worker(f4,(rdy3,tmp3),(rdy4,tmp4))
  and () = pipe_worker(f5,(rdy4,tmp4),(rdy5,tmp5))
  and () = pipe_worker(f6,(rdy5,tmp5),(rdy6,tmp6))
  and () = pipe_worker(f7,(rdy6,tmp6),(rdy7,tmp7))
  and () = pipe_worker(f8,(rdy7,tmp7),(rdy8,tmp8))
  and () = pipe_worker(f9,(rdy8,tmp8),(rdy9,tmp9))
  in k(tmp9)
;;


let rec wait(n) =
  if n <= 1 then () else wait(n-1) ;;


let collatz(n) =
  let rec loop(n,t) =
    if n = 1 then t else
    if n mod 2 = 0 then loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(n,1) ;;


let f1 x = if x mod 2 = 0 then (wait(10);x+1) else x+1 ;;
let f2 x = collatz (x+1) ;;
let f3 x = wait(20); x;;
let r = create<1>() ;;
let f4 x = set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);
set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1);set(r,0,1); x;;

let counter () =
  reg (fun c -> c + 1) last 0 ;;

let pipe1_seq((f1,a),k) =
  let _ = (a : `A array<'N>) in (* type constraint *)
  let dst = create<'N>() in
  for i = 0 to length a - 1 do set(dst,i,f1((get(a,i)))) done;
  k(dst);;

let pipe2_seq((f1,f2,a),k) =
  let _ = (a : `A array<'N>) in (* type constraint *)
  let dst = create<'N>() in
  for i = 0 to length a - 1 do set(dst,i,f2(f1(get(a,i)))) done;
  k(dst);;

let pipe3_seq((f1,f2,f3,a),k) =
  let _ = (a : `A array<'N>) in (* type constraint *)
  let dst = create<'N>() in
  for i = 0 to length a - 1 do set(dst,i,f3(f2(f1(get(a,i))))) done;
  k(dst);;


let pipe4_seq((f1,f2,f3,f4,a),k) =
  let _ = (a : `A array<'N>) in (* type constraint *)
  let dst = create<'N>() in
  for i = 0 to length a - 1 do set(dst,i,f4(f3(f2(f1(get(a,i)))))) done;
  k(dst);;

let pipe5_seq((f1,f2,f3,f4,f5,a),k) =
  let _ = (a : `A array<'N>) in (* type constraint *)
  let dst = create<'N>() in
  for i = 0 to length a - 1 do set(dst,i,f5(f4(f3(f2(f1(get(a,i))))))) done;
  k(dst);;

let pipe6_seq((f1,f2,f3,f4,f5,f6,a),k) =
  let _ = (a : `A array<'N>) in (* type constraint *)
  let dst = create<'N>() in
  for i = 0 to length a - 1 do set(dst,i,f6(f5(f4(f3(f2(f1(get(a,i)))))))) done;
  k(dst);;

let pipe7_seq((f1,f2,f3,f4,f5,f6,f7,a),k) =
  let _ = (a : `A array<'N>) in (* type constraint *)
  let dst = create<'N>() in
  for i = 0 to length a - 1 do set(dst,i,f7(f6(f5(f4(f3(f2(f1(get(a,i))))))))) done;
  k(dst);;


let pipe8_seq((f1,f2,f3,f4,f5,f6,f7,f8,a),k) =
  let _ = (a : `A array<'N>) in (* type constraint *)
  let dst = create<'N>() in
  for i = 0 to length a - 1 do set(dst,i,f8(f7(f6(f5(f4(f3(f2(f1(get(a,i)))))))))) done;
  k(dst);;

let init_array(a) = 
  for i = 0 to length(a) - 1 do set(a,i, i+1) done ;;

let main () =
  let cy = counter () in
  let ((),rdy) =
    exec
      let t = create<100>() in
      (init_array t;
       pipe8(collatz,f3,f1,collatz,f1,f1,f1,f1,t) @ fun _ -> ())
  default () in
if rdy then (print_string " cy=";
             print_int (cy-1);
             print_newline()) ;;

