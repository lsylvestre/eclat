let static tab = 100^100;;

let static size = 10 ;;


let mapi f ~n ~src ~dst =
  let rec loop(i) =
    if i <= src.length - n then ( 
      for ofs = 0 to n do 
        let pos = i + ofs in
        dst[pos] <- f (pos,src[pos])
      done;
      loop(i+n)
    ) 
  in
  loop(0) ;;


let map f ~n ~src ~dst =
  let f_indexed (i,x) = f(x) in
  mapi f_indexed ~n:n ~src:src ~dst:dst ;; 


let reduce (f,init) ~src ~depth =
  let rec loop(acc,i) =
    if i <= src.length - depth
    then let g (ofs,acc) = f (acc, src[i+ofs]) in
         let acc = generate g acc ~depth:depth in
         loop(acc,i+depth)
    else acc
  in loop(init,0) ;;

let zip_with_i f ~n ~src1 ~src2 ~dst =
  let rec loop(i) =
    if i <= src1.length - n then ( 
      for ofs = 0 to n do 
        let pos = i + ofs in
        dst[pos] <- f (pos,(src1[pos],src2[pos]))
      done;
      loop(i+n)
    ) 
  in
  loop(0) ;;

let zip_with f ~n ~src1 ~src2 ~dst =
  let f_indexed (i,x) = f(x) in
  zip_with_i f_indexed ~n:n ~src1:src1 ~src2:src2 ~dst:dst ;; 

let counter () =
  reg (fun c -> c + 1) last 0 ;;

let rec pause () = () ;;

let rec loop(id,n) = 
  print_int id; if n < 1 then () else
  let _ = tab[id] <- 0 in
  let _ = tab[id] in
  pause ();
  pause ();
  pause ();
    pause ();
  let _ = tab[id] <- 0 in
  (* let _ = tab[id+1] in
  let _ = tab[id+3] in*)
  loop(id,n-1) ;;

let main () =
  let x1 = loop(1,100) 
  and x2 = loop(2,100)
  and x3 = loop(3,100) 
  and x4 = loop(4,100) 
  and x5 = loop(5,100)
  and x6 = let _ = tab[55]in print_string "boom!!" in
  print_string "foo";
  print_newline();;
  () ;;

