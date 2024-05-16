
let rec safe (d,x,lst) =
  if is_int(lst) then val_true else
  let q = long_val(get_field(lst,0)) in
  let t = get_field(lst,1) in
  if (not (x == q)) &&
     (not (x == q+d)) &&
     (not (x == q-d)) then safe (d+1,x,t) else val_false ;;

let ok ((lst,_),env) =
  let v = if is_int(lst) then val_true else
          let q = long_val(get_field(lst,0)) in
          let t = get_field(lst,1) in
          safe(1,q,t)
  in
  (v,env)  ;;
