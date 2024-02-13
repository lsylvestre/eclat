let print_val (v:value) =
  print_int (long_val v);
  print_string "<";
  (if is_int(v) then print_string "int" else print_string "ptr");
  print_string ">" ;;


let print_stack(sp) =
  print_string "stack: [";
  let rec w(i) =
    if i >= sp then () else
    let v = ram[i] in (
      print_int (long_val(v));
      print_string "|";
      w(i+1)
    )
  in
  w(stack_start);
  print_string "]";
  print_newline () ;;

