(* let stack_update(cb,i,q) =
  print_int i;
  let cb' = Array.copy cb in
  print_int 12345;
  print_int i;
  print_int 989;
  print_int q;
  cb'.(i) <- q;
  cb' ;;*)

let stack_update(cb,i,q) =
  let cb' = Array.copy cb in
  cb'.(i) <- q;
  cb' ;;

let stack_nth(cb,i) = cb.(i) ;;
let add_solution(stack, acc) = Array.copy stack :: acc ;;
