let static m = 1^100^100;;

(* let rec sum_i (acc,j,i) =
  if i >= 100 then acc else sum_i(acc+m.(i).(j),j,i+1) ;;
*)

(* let rec sum_i (acc,j,i) =
  generate (fun (i,acc) -> acc + m.(i).(j)) acc ~depth:100 ;;

let rec sum_j (acc,j,i) =
  if j >= 100 then acc else sum_j(sum_i(acc,j,i),j+1,i) ;;
*)

let fill(m) =
  let rec loop(i) =
    if i >= 100 then () else
    (for j = 0 to 99 do
      m.(i).(j) <- 42
    done;
    loop(i+1))
  in loop(0) ;;

let inc(m) =
  let rec loop(i) =
    if i >= m.(0).size then () else
    (for j = 0 to 99 do
      m.(i).(j) <- m.(i).(j) + 1
    done;
    loop(i+1))
  in loop(0) ;;

  let counter () =
  reg (fun c -> c + 1) last 0 ;;


let main (u:int<12>) : int<58> =
  let c = counter () in
  let (n,rdy) =
  exec 
    m.(1).(1) <- 0;
    inc(m);
    print_int(m.(1).(1));0
    (* sum_j(0,0,0) *)
  default 0
  in 
  (if rdy then (print_newline() ; 
                print_string "cy:"; 
                print_int c; print_newline ())) ;
  resize_int<58>(n) ;;

