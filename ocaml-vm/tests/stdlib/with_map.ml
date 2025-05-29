
module M = Map.Make(struct 
             type t = int 
             let compare x y =
               if x < y then -1 else if x > y then 1 else 0
             end);;

let m = M.add 42 43 M.empty ;;

print_int (M.find 42 m) ;;
