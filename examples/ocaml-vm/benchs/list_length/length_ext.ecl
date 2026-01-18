
let len (lst) =
  let rec aux (lst,acc) =
      if is_int(lst) then acc else
      aux(get_field(lst,1), acc + 1)
  in

  aux(lst,0);;

let rec length_ext ((lst,_),env) =
    print_string "toto";
  let v = val_long(len(lst)) in
   print_string "titi";
  (v, env) ;;
