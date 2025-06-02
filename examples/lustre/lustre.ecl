
let fact n =
  let rec loop(acc,n) =
    if n < 2 then acc else loop(acc * n, n-1)
  in loop(1,n) ;;

let sum i = s where
  rec s = pre_s + i
  and pre_s = 0 fby s ;;

let sum_fact(n) = o where
  rec (x,rdy) = exec fact(n)
  and y = pre_y + x when rdy
  and pre_y = 0 fby y when rdy
  and o = merge rdy y (42 when not rdy) ;;

let main(i) =
  print_int (sum_fact(i));
  print_newline () ;;


(* ./eclat -arg="3;3;3;3;3;3;5;5;5;1" ../examples/lustre/lustre.ecl *)
 
