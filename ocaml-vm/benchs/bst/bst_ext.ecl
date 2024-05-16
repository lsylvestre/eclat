let rec mem (x,tree) =
  if is_int(tree) then val_false else
  let q = long_val(get_field(tree,0)) in
     print_int q;
  if q < x then mem(x,get_field(tree,1)) else
  if q > x then mem(x,get_field(tree,2))
           else val_true ;;

let rec search_ext ((x,(tree,_)),env) =
  let v = mem (long_val(x),tree) in
  (v, env) ;;

