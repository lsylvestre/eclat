let static tab = 100^100;;

let static size = 10 ;;

let mapi f ~n ~src ~dst =
  let x = src[n] in () ;;

let map f ~n ~src ~dst =
  let f_indexed (i,x) = f(x) in
  mapi f_indexed ~n:n ~src:src ~dst:dst ;; 


let counter () =
  reg (fun c -> c + 1) last 0 ;;

let main () =
  let cy = counter () in
  exec
    let () = mapi (fun (i,x) -> x+1) ~n:size ~src:tab ~dst:tab in
    let () = map (fun x -> x+1) ~n:size ~src:tab ~dst:tab in
    ()
  default ();;

