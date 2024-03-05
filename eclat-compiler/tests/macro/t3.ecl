let static tab = 100^10;;

let static size = 8 ;;

let reduce (f,acc) ~n ~src =
  let rec loop(i,acc) =
    if i >= src.length - n then acc else
    let g (ofs,acc) = acc+1 (* f(src[i+ofs],acc)*) in
    let acc = generate(g,acc) ~n:n in
    loop(i+n,acc)
  in
  loop(0,acc) ;;

let counter () =
  reg (fun c -> c + 1) last 0 ;;

let main () =
  let cy = counter () in
  exec
    let u = reduce ((+),0) ~n:size ~src:tab in
    print_int u;
    ()
  default ();;

