let static t = 0^100 ;;
let static d = 4 ;;


let await (i,reset) =
  let step s = (s or i) & not reset in
  reg step last false ;;
  
let fby (a,b) =
  let step (x,_) = (b,x) in
  let (_,x) = reg step last (a,b) in x
;;

let edge i = not (fby(false,i)) & i ;;

let abro (a,b,r) = edge (await (a,r) & await(b,r)) ;;

let abcro (a,b,c,r) = abro (abro(a,b,r),c,r) ;;


let map_inplace (f,n,tab) =
  let rec loop(i) =
    if i <= tab.length - n then ( 
      for ofs = 0 to n-1 do
        let pos = i + ofs in
        tab.(pos) <- f (tab.(pos))
      done;
      loop(i+n)) 
  in loop(0) ;;


let main (a,b,c,i) =
  let s = reg (fun s -> s + i) last 0 in
  let ((),rdy) = exec map_inplace((fun x -> x + s),d,t) default () in
  abcro(a,b,c,rdy) ;;