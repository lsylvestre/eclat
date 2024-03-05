let static tab = 10^100 ;;

let map(f,n,src,dst) =
  let rec loop(i) =
    if i <= src.length - n then ( 
      for ofs = 0 to n-1 do
        let pos = i + ofs in
        dst.(pos) <- f(src.(pos))
      done;
      loop(i+n)
    ) 
  in
  loop(0) ;;

let reduce (f,init,src,depth) =
  let rec loop(acc,i) =
    if i <= src.length - depth
    then let g (ofs,acc) = f (acc, src.(i+ofs)) in
         let acc = generate g acc ~depth:depth in
         loop(acc,i+depth)
    else acc
  in loop(init,0) ;;

let succ n = n + 1 ;;

let main () =
  let size = 8 in
  map(succ,size,tab,tab); (* map avec 8 branches parallèles *)
  let s = reduce((+),0,tab,2) in
  print_int(s) ;;