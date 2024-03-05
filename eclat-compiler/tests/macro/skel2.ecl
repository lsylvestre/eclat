let static tab = 100^100 ;;

let size = 10 ;;

let mapi (f,n,src,dst) =
  let rec loop(i) =
    if i <= src.length - n then ( 
      for ofs = 0 to n-1 do
        let pos = i + ofs in
        dst.(pos) <- f (pos,src.(pos))
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

let g (_,x) = x + 1;;

let main() = 
  mapi(g,size,tab,tab);
  print_int (reduce((+),0,tab,2)) ;;